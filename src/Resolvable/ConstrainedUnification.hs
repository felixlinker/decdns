{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier(..)
  , Apply(..)
  , Neqs
  , toList
  , asPredicates
  , fromList
  , simplify
  , identity
  , fromUnifier
  , applyRule
  , addConstraint
  , extendUnifier ) where

import Control.Applicative (Alternative (..))
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(bimap))

type Neqs = M.Map Term (S.Set Term)

fromListUnchecked :: [(Term, Term)] -> Neqs
fromListUnchecked = foldr (uncurry addConstraintUnchecked) M.empty

fromList :: [(Term, Term)] -> Maybe Neqs
fromList = fmap (foldr (uncurry addConstraintUnchecked) M.empty) . simplifyL

toList :: Neqs -> [(Term, Term)]
toList = concatMap (\(vn, ts) -> map (vn,) (S.toList ts)) . M.toList

asPredicates :: Neqs -> [Term]
asPredicates = map (uncurry (\=)) . toList

data ConstrainedUnifier =
  CUnifier  { unif :: U.Unifier
            , constraints :: Neqs }

instance Show ConstrainedUnifier where
  show (CUnifier u neqs) = show u ++ " s.t. " ++ (if null cond then "True" else cond)
    where
      cond :: String
      cond = intercalate ", " $ map show $ asPredicates neqs

identity :: ConstrainedUnifier
identity = CUnifier U.identity M.empty

fromUnifier :: U.Unifier -> ConstrainedUnifier
fromUnifier = (`CUnifier` M.empty)

applyRule :: ConstrainedUnifier -> Rule a -> Maybe (Rule a, ConstrainedUnifier)
applyRule cu (Rule ann h bs) = do
  (bs', cu') <- foldr applyBody (Just ([], cu)) bs
  return (Rule ann (apply cu' h) bs', cu')
  where
    applyBody :: Term -> Maybe ([Term], ConstrainedUnifier) -> Maybe ([Term], ConstrainedUnifier)
    applyBody t acc = do
      (bs', u) <- acc
      (t', u') <- case toView t of
            TVNeq a b -> (\u' -> (apply u' t, u')) <$> addConstraint a b u
            _ -> Just (apply u t, u)
      return (t':bs', u')

addConstraintUnchecked :: Term -> Term -> Neqs -> Neqs
addConstraintUnchecked k v = M.alter (Just . maybe (S.singleton v) (S.insert v)) k

addConstraint :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
addConstraint a b (CUnifier u neqs) =
  let a' = U.apply u a
      b' = U.apply u b
  in CUnifier u . foldr (uncurry addConstraintUnchecked) neqs <$> toMaybe (simplifyNeq a' b')

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  CUnifier u' . fromListUnchecked <$> simplifyL (map (bimap (U.apply u') (U.apply u')) $ toList neqs)

data Simplification a = Equal | Unequal | Undet a
  deriving (Eq, Ord, Show)

instance Functor Simplification where
  fmap _ Equal = Equal
  fmap _ Unequal = Unequal
  fmap f (Undet a) = Undet (f a)

instance Applicative Simplification where
  pure = Undet
  Equal <*> _ = Equal
  _ <*> Equal = Equal
  Unequal <*> _ = Unequal
  _ <*> Unequal = Unequal
  Undet f <*> Undet a = Undet (f a)

instance Alternative Simplification where
  empty = Unequal
  Unequal <|> a = a
  Equal <|> a = a
  a@(Undet _) <|> _ = a

toMaybe :: Simplification a -> Maybe [a]
toMaybe Equal = Nothing
toMaybe Unequal = Just []
toMaybe (Undet a) = Just [a]

simplify :: Neqs -> Maybe Neqs
simplify = fmap fromListUnchecked . simplifyL . toList

simplifyL :: [(Term, Term)] -> Maybe [(Term, Term)]
simplifyL = fmap concat . mapM (toMaybe . uncurry simplifyNeq)

simplifyArgs :: [Term] -> [Term] -> Simplification [(Term, Term)]
-- Arguments are equal, i.e., contradictory
simplifyArgs [] [] = Equal
simplifyArgs [a] [b] = (:[]) <$> simplifyNeq a b
-- Arguments are unequal, i.e., constraint is redundant
simplifyArgs [] (_:_) = Unequal
simplifyArgs (_:_) [] = Unequal
simplifyArgs (a:ta) (b:tb) = case simplifyNeq a b of
  Equal -> simplifyArgs ta tb
  Unequal -> Unequal
  Undet (a', b') -> case simplifyArgs ta tb of
    Unequal -> Unequal
    Equal -> Undet [(a', b')]
    Undet neqs -> Undet ((a', b'):neqs)

listOrSingleton :: [Term] -> Term
listOrSingleton [a] = a
listOrSingleton l = list l

simplifyNeq :: Term -> Term -> Simplification (Term, Term)
simplifyNeq a@(TVar _) b
  | a /= b = Undet (a, b)
  | otherwise = Equal
-- NOTE: a below cannot be a variable
simplifyNeq a b@(TVar _) = simplifyNeq b a
-- NOTE: b below cannot be a variable
simplifyNeq a b = case a of
  -- We can simplify all below cases to either dropping the inequality, as the
  -- terms are unequal, or deriving a contradiction.
  TEmptyList      -> decideNonVar
  (TAtomQuoted _) -> decideNonVar
  (TAtom _)       -> decideNonVar
  (TString _)     -> decideNonVar
  (TInt _)        -> decideNonVar
  (TComplex _)    -> decideNonVar
  TFunctor na argsa -> case b of
    TFunctor nb argsb
      -- If the functor name is the same, we can reduce inequality of the
      -- functor to the inequality of its list of arguments.
      | na == nb -> bimap listOrSingleton listOrSingleton . unzip <$> simplifyArgs argsa argsb
    _ -> Unequal
  where
    decideNonVar :: Simplification a
    decideNonVar
      | a == b    = Equal
      | otherwise = Unequal

class Apply a where
  apply :: ConstrainedUnifier -> a -> a

instance Apply a => Apply [a] where
  apply cu = map (apply cu)

instance Apply Term where
  apply (CUnifier u _) = U.apply u

instance Apply (Rule a) where
  apply cu@(CUnifier _ neqs) (Rule ann (apply cu -> h') (apply cu -> b')) =
    Rule ann h' (asPredicates neqs ++ b')
