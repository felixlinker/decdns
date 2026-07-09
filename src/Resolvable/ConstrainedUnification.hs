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

import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(bimap))
import Util.List (zipStrict)

type Neqs = M.Map String (S.Set Term)

fromListUnchecked :: [(String, Term)] -> Neqs
fromListUnchecked = foldr (uncurry addConstraintUnchecked) M.empty

fromList :: [(Term, Term)] -> Maybe Neqs
fromList = fmap (foldr (uncurry addConstraintUnchecked) M.empty) . simplifyL

toList :: Neqs -> [(Term, Term)]
toList = concatMap (\(vn, ts) -> map (TVar vn,) (S.toList ts)) . M.toList

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

addConstraintUnchecked :: String -> Term -> Neqs -> Neqs
addConstraintUnchecked k v = M.alter (Just . maybe (S.singleton v) (S.insert v)) k

addConstraint :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
addConstraint a b (CUnifier u neqs) =
  let a' = U.apply u a
      b' = U.apply u b
  in CUnifier u . foldr (uncurry addConstraintUnchecked) neqs <$> simplifyNeq a' b'

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  CUnifier u' . fromListUnchecked <$> simplifyL (map (bimap (U.apply u') (U.apply u')) $ toList neqs)

simplify :: Neqs -> Maybe Neqs
simplify = fmap fromListUnchecked . simplifyL . toList

simplifyL :: [(Term, Term)] -> Maybe [(String, Term)]
simplifyL = fmap concat . mapM (uncurry simplifyNeq)

simplifyNeq :: Term -> Term -> Maybe [(String, Term)]
simplifyNeq TWildcard _ = Just []
simplifyNeq _ TWildcard = Just []
simplifyNeq a@(TVar vn) b = guard (a /= b) $> [(vn, b)]
-- NOTE: a below cannot be a variable
simplifyNeq a b@(TVar _) = simplifyNeq b a
simplifyNeq a b = case a of
  TEmptyList      -> contradictionWhenEq
  (TAtomQuoted _) -> contradictionWhenEq
  (TAtom _)       -> contradictionWhenEq
  (TString _)     -> contradictionWhenEq
  (TInt _)        -> contradictionWhenEq
  (TComplex _)    -> contradictionWhenEq
  TFunctor na argsa -> case b of
    TFunctor nb argsb
      | na == nb -> maybe redundant rec (zipStrict argsa argsb)
    _ -> redundant
  where
    redundant :: Maybe [a]
    redundant = Just []

    contradictionWhenEq :: Maybe [(String, Term)]
    contradictionWhenEq
      | a == b    = Nothing
      | otherwise = redundant

    rec :: [(Term, Term)] -> Maybe [(String, Term)]
    rec [] = Nothing
    rec zipped = simplifyL zipped

class Apply a where
  apply :: ConstrainedUnifier -> a -> a

instance Apply a => Apply [a] where
  apply cu = map (apply cu)

instance Apply Term where
  apply (CUnifier u _) = U.apply u

instance Apply (Rule a) where
  apply cu@(CUnifier _ neqs) (Rule ann (apply cu -> h') (apply cu -> b')) =
    Rule ann h' (asPredicates neqs ++ b')
