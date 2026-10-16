{-# LANGUAGE ViewPatterns #-}
module Resolvable.Unification
  ( Unifier(..)
  , (~>)
  , Unify(..)
  , Apply(..)
  , identity
  , termIdentity
  , toList
  , restrict
  , isRenaming
  , nonTrivial
  , filterRenamings
  , unify
  , unifies
  , equivModuloRenaming
  , awayT
  , away ) where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(bimap), first)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)

import Resolvable.Prolog.Core
import Util.List (zipWithStrict)
import Util.Fresh

newtype Unifier = Unifier (M.Map Term Term)
  deriving Eq

(~>) :: Unifier -> Unifier -> Unifier
Unifier m1 ~> u = Unifier $ M.map (apply u) m1

instance Show Unifier where
  show (Unifier m) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList m)) ++ "}"

class Apply t where
  apply :: Unifier -> t -> t

instance Apply t => Apply [t] where
  apply u = map (apply u)

instance (Apply a, Apply b) => Apply (a, b) where
  apply u = bimap (apply u) (apply u)

instance Apply (Rule a) where
  apply u (Rule ann h b) = Rule ann (apply u h) (apply u b)

instance (Apply a, Ord a) => Apply (S.Set a) where
  apply u = S.map (apply u)

identity :: Unifier
identity = Unifier M.empty

termIdentity :: Term -> Unifier
termIdentity t =
  let vs = S.toList $ vars t
  in Unifier $ M.fromList $ zip vs vs

singleton :: Term -> Term -> Unifier
singleton v@(TVar _) t = Unifier (M.singleton v t)
singleton _ _ = undefined

toList :: Unifier -> [(Term, Term)]
toList (Unifier u) = M.toList u

restrict :: S.Set Term -> Unifier -> Unifier
restrict dom (Unifier m) = Unifier $ M.filterWithKey (\k _ -> S.member k dom) m

isRenaming :: Unifier -> Bool
isRenaming (Unifier (M.elems -> ts)) = all isVar ts && not (hasDuplicates ts)
  where
    hasDuplicates :: Ord a => [a] -> Bool
    hasDuplicates = isNothing . foldr (\a -> (>>= S.alterF insertP a)) (Just S.empty)

    insertP :: Bool -> Maybe Bool
    insertP False = Just True  -- Insert if not found
    insertP True = Nothing     -- Abort if found

nonTrivial :: Unifier -> Bool
nonTrivial (Unifier m) = not $ all isVar m

-- Remove all renamings from the unifier.
-- For example, [X -> Z, Y -> Z] becomes [Y -> X].
filterRenamings :: Unifier -> Unifier
filterRenamings (Unifier m) = fst $ foldr foldf (identity, identity) $ M.toList m
  where
    foldf :: (Term, Term) -> (Unifier, Unifier) -> (Unifier, Unifier)
    foldf (from, to) (acc@(Unifier m'), aux@(Unifier m''))
      -- Apply the unifier to @to@ as it either does or may contain the dropped
      -- renaming. Need not apply unifier to @from@ because we always store
      -- unifier in a form where there are no chains of renamings.
      | to `M.member` m'' || not (isVar to) = (Unifier (M.insert from (apply aux to) m'), aux)
      -- Drop the mapping from @from@ to @to@ and stores its inverse in the
      -- auxiliary unifier.
      | otherwise = (acc, Unifier (M.insert to from m''))

instance Apply Term where
  apply u (TFunctor op (map (apply u) -> args)) = TFunctor op args
  apply _ TEmptyList = TEmptyList
  apply (Unifier m) v@(TVar _) = fromMaybe v (M.lookup v m)
  apply _ t@(TAtom _) = t
  apply _ t@(TString _) = t
  apply _ t@(TInt _) = t
  apply _ t@(TComplex _) = t

unify :: Unify a => a -> a -> Maybe Unifier
unify t1 t2 = extendUnifier t1 t2 identity

unifies :: Unify a => a -> a -> Bool
unifies t1 t2 = isJust (unify t1 t2)

equivModuloRenaming :: Unify a => a -> a -> Bool
equivModuloRenaming t1 t2 = maybe False isRenaming (unify t1 t2)

class Unify a where
  extendUnifier :: a -> a -> Unifier -> Maybe Unifier

instance Unify Term where
  -- Call @apply u@ on @to@ so that the unifier stays normal
  extendUnifier from to u@(Unifier m) = unif (apply u from) (apply u to)
    where
      unifList :: [Term] -> [Term] -> Maybe Unifier
      unifList l1 l2 = foldr (=<<) (Just u) =<< zipWithStrict extendUnifier l1 l2

      unifVar :: Term -> Term -> Maybe Unifier
      unifVar v@(TVar _) t
        | t == v = Just u
        | subterm v t = Nothing
        | otherwise   =
          let base = Unifier $ M.insert v t $ M.map (apply (singleton v t)) m
          in  maybe (Just base) (\t' -> extendUnifier t' t u) (M.lookup v m)
      unifVar _ _ = undefined

      unif :: Term -> Term -> Maybe Unifier
      unif v@(TVar _) t = unifVar v t
      unif t v@(TVar _) = unifVar v t
      unif (TFunctor op1 args1) (TFunctor op2 args2)
        | op1 == op2 = unifList args1 args2
        | otherwise  = Nothing
      unif (TFunctor _ _) _ = Nothing
      unif TEmptyList TEmptyList = Just u
      unif TEmptyList _ = Nothing
      unif (TAtom a) (TAtom b)
        | a == b    = Just u
        | otherwise = Nothing
      unif (TAtom _) _ = Nothing
      unif (TString a) (TString b)
        | a == b    = Just u
        | otherwise = Nothing
      unif (TString _) _ = Nothing
      unif (TInt a) (TInt b)
        | a == b = Just u
        | otherwise = Nothing
      unif (TInt _) _ = Nothing
      unif (TComplex d1) (TComplex d2)
        | d1 == d2 = Just u
        | otherwise = Nothing
      unif (TComplex _) _ = Nothing

awayT :: (Traversable t, Monad m) => t Term -> FreshT m (t Term, Unifier)
awayT t = do
    (t', m) <- St.runStateT (traverse (mapVarsF rename) t) M.empty
    return (t', Unifier $ M.fromList $ map (bimap TVar TVar) $ M.toList m)
  where
    rename :: Monad m => String -> St.StateT (M.Map String String) (FreshT m) String
    rename varName = do
      mapped <- St.gets (M.lookup varName)
      maybe freshName return mapped
      where
        freshName :: Monad m => St.StateT (M.Map String String) (FreshT m) String
        freshName = do
          n <- lift $ fresh "V"
          St.modify (M.insert varName n)
          return n

away :: Monad m => Term -> FreshT m (Term, Unifier)
away = fmap (first runIdentity) . awayT . Identity
