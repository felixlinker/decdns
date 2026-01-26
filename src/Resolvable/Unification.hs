{-# LANGUAGE ViewPatterns #-}
module Resolvable.Unification
  ( Unifier(..)
  , identity
  , apply
  , unify
  , unifies
  , matchHead
  , extendUnifier ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)

import Resolvable.Prolog

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

newtype Unifier = Unifier (M.Map Term Term)
  deriving Eq

instance Show Unifier where
  show (Unifier m) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList m)) ++ "}"

identity :: Unifier
identity = Unifier M.empty

singleton :: Term -> Term -> Unifier
singleton v@(TVar _) t = Unifier (M.singleton v t)
singleton _ _ = undefined

apply :: Unifier -> Term -> Term
apply u (TOp op (map (apply u) -> args)) = TOp op args
apply (Unifier m) v@(TVar _) = fromMaybe v (M.lookup v m)
apply _ TWildcard = TWildcard
apply _ t@(TAtomQuoted _) = t
apply _ t@(TAtom _) = t
apply u (TList (map (apply u) -> ts) t) = case fmap (apply u) t of
  Nothing -> TList ts Nothing
  Just (TList ts' t') -> TList (ts ++ ts') t'
  t' -> TList ts t'
apply _ t@(TString _) = t
apply u (TPred n (map (apply u) -> ts)) = TPred n ts

unify :: Term -> Term -> Maybe Unifier
unify t1 t2 = extendUnifier t1 t2 identity

unifies :: Term -> Term -> Bool
unifies t1 t2 = isJust (unify t1 t2)

matchHead :: Term -> Rule a -> Bool
matchHead t = unifies t . rhead

extendUnifier :: Term -> Term -> Unifier -> Maybe Unifier
-- Call @apply u@ on @to@ so that the unifier stays normal
extendUnifier from to u@(Unifier m) = unif from (apply u to)
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
    unif v@(TVar _) t = unifVar v t
    unif t v@(TVar _) = unifVar v t
    unif (TOp op1 args1) (TOp op2 args2)
      | op1 == op2 = unifList args1 args2
      | otherwise  = Nothing
    unif (TOp _ _) _ = Nothing
    unif (TAtomQuoted a) (TAtomQuoted b)
      | a == b    = Just u
      | otherwise = Nothing
    unif (TAtomQuoted _) _ = Nothing
    unif (TAtom a) (TAtom b)
      | a == b    = Just u
      | otherwise = Nothing
    unif (TAtom _) _ = Nothing
    unif (TList hs1 t1) (TList hs2 t2) = do
      unifTList hs1 hs2 t1 t2 u
      where
        unifTList :: [Term] -> [Term] -> Maybe Term -> Maybe Term -> Unifier -> Maybe Unifier
        unifTList []        []        Nothing Nothing u' = Just u'
        unifTList []        (_:_)     Nothing _ _ = Nothing
        unifTList (_:_)     []        _ Nothing _ = Nothing
        unifTList []        hs2'      (Just v@(TVar _)) t2' u' = extendUnifier v (TList hs2' t2') u'
        unifTList hs1'      []        t1' (Just v@(TVar _)) u' = extendUnifier v (TList hs1' t1') u'
        -- Do not support any other terms in tails other than variable
        unifTList []        _         (Just _) _ _ = Nothing
        unifTList _         []        _ (Just _) _ = Nothing
        unifTList (h1:hs1') (h2:hs2') t1' t2' u' =
          extendUnifier h1 h2 u' >>= unifTList hs1' hs2' t1' t2'
    unif (TList _ _) _ = Nothing
    unif (TString a) (TString b)
      | a == b    = Just u
      | otherwise = Nothing
    unif (TString _) _ = Nothing
    unif (TPred h1 args1) (TPred h2 args2)
      | h1 == h2 = unifList args1 args2
      | otherwise = Nothing
    unif (TPred _ _) _ = Nothing
