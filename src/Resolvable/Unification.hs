module Resolvable.Unification
  ( Unifier(..)
  , unify
  , unifies
  , matchHead
  , extendUnifier ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Maybe (isNothing)

import Resolvable.Prolog

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

data Unifier = Unifier (M.Map Term Term) | NoUnifier

instance Show Unifier where
  show NoUnifier = "no unifier"
  show (Unifier m) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList m)) ++ "}"

instance Semigroup Unifier where
  NoUnifier <> _ = NoUnifier
  _ <> NoUnifier = NoUnifier
  u1 <> (Unifier u2) = M.foldrWithKey updateMapping u1 u2
    where
      updateMapping :: Term -> Term -> Unifier -> Unifier
      updateMapping _ _ NoUnifier = NoUnifier
      updateMapping k v u@(Unifier m) =
        let v' = M.lookup k m
        in maybe (Unifier $ M.insert k v m) extend v'
        where
          extend :: Term -> Unifier
          extend v' = extendUnifier u v' v

instance Monoid Unifier where
  mempty = Unifier M.empty

unify :: Term -> Term -> Unifier
unify = extendUnifier mempty

unifies :: Term -> Term -> Bool
unifies t1 t2 = case unify t1 t2 of
  NoUnifier -> False
  _         -> True

matchHead :: Term -> Rule a -> Bool
matchHead t = unifies t . rhead

unifyList :: Unifier -> [Term] -> [Term] -> Unifier
unifyList u t1 t2 = maybe NoUnifier mconcat (zipWithStrict (extendUnifier u) t1 t2)

extendUnifier :: Unifier -> Term -> Term -> Unifier
extendUnifier NoUnifier _ _ = NoUnifier
extendUnifier u TWildcard _ = u
extendUnifier u v@(TVar _) t
  | t == v = u
  | otherwise   = Unifier (M.singleton v t)
extendUnifier u (TOp op1 args1) (TOp op2 args2)
  | op1 == op2 = unifyList u args1 args2
  | otherwise  = NoUnifier
extendUnifier _ (TOp _ _) _ = NoUnifier
extendUnifier u (TAtomQuoted a) (TAtomQuoted b)
  | a == b    = u
  | otherwise = NoUnifier
extendUnifier _ (TAtomQuoted _) _ = NoUnifier
extendUnifier u (TAtom a) (TAtom b)
  | a == b    = u
  | otherwise = NoUnifier
extendUnifier _ (TAtom _) _ = NoUnifier
extendUnifier u (TList hs1 t1) (TList hs2 t2) = go u hs1 hs2 t1 t2
  where
    go :: Unifier -> [Term] -> [Term] -> Maybe String -> Maybe String -> Unifier
    go u' []        hs2'      Nothing t2'
      | null hs2' && isNothing t2' = u'
      | otherwise = NoUnifier
    go u' []        hs2'      (Just v) t2' =
      extendUnifier u' (TVar v) (TList hs2' t2')
    go _  _         []        _ _ = NoUnifier
    go u' (h1:hs1') (h2:hs2') t1' t2' =
      let u'' = extendUnifier u' h1 h2
      in go u'' hs1' hs2' t1' t2'
extendUnifier _ (TList _ _) _ = NoUnifier
extendUnifier u (TString a) (TString b)
  | a == b    = u
  | otherwise = NoUnifier
extendUnifier _ (TString _) _ = NoUnifier
extendUnifier u (TPred h1 args1) (TPred h2 args2)
  | h1 == h2 = unifyList u args1 args2
  | otherwise = NoUnifier
extendUnifier _ (TPred _ _) _ = NoUnifier
