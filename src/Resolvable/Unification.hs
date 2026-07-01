{-# LANGUAGE ViewPatterns #-}
module Resolvable.Unification
  ( Unifier(..)
  , Apply(..)
  , identity
  , toList
  , restrict
  , isRenaming
  , unify
  , unifies
  , equivModuloRenaming
  , matchHead
  , extendUnifier ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)

import Resolvable.Prolog.Core
import Util.List (zipWithStrict)
import Data.Bifunctor (Bifunctor(bimap))

newtype Unifier = Unifier (M.Map Term Term)
  deriving Eq

instance Show Unifier where
  show (Unifier m) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList m)) ++ "}"

class Apply t where
  apply :: Unifier -> t -> t

instance Apply t => Apply [t] where
  apply u = map (apply u)

instance (Apply a, Apply b) => Apply (a, b) where
  apply u = bimap (apply u) (apply u)

identity :: Unifier
identity = Unifier M.empty

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

instance Apply Term where
  apply u (TFunctor op (map (apply u) -> args)) = TFunctor op args
  apply _ TEmptyList = TEmptyList
  apply (Unifier m) v@(TVar _) = fromMaybe v (M.lookup v m)
  apply _ TWildcard = TWildcard
  apply _ t@(TAtomQuoted _) = t
  apply _ t@(TAtom _) = t
  apply _ t@(TString _) = t
  apply _ t@(TInt _) = t
  apply _ t@(TComplex _) = t

unify :: Term -> Term -> Maybe Unifier
unify t1 t2 = extendUnifier t1 t2 identity

unifies :: Term -> Term -> Bool
unifies t1 t2 = isJust (unify t1 t2)

equivModuloRenaming :: Term -> Term -> Bool
equivModuloRenaming t1 t2 = maybe False isRenaming (unify t1 t2)

matchHead :: Term -> Rule a -> Bool
matchHead t = unifies t . rhead

extendUnifier :: Term -> Term -> Unifier -> Maybe Unifier
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
    unif TWildcard _ = Just u
    unif _ TWildcard = Just u
    unif v@(TVar _) t = unifVar v t
    unif t v@(TVar _) = unifVar v t
    unif (TFunctor op1 args1) (TFunctor op2 args2)
      | op1 == op2 = unifList args1 args2
      | otherwise  = Nothing
    unif (TFunctor _ _) _ = Nothing
    unif TEmptyList TEmptyList = Just u
    unif TEmptyList _ = Nothing
    unif (TAtomQuoted a) (TAtomQuoted b)
      | a == b    = Just u
      | otherwise = Nothing
    unif (TAtomQuoted _) _ = Nothing
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
