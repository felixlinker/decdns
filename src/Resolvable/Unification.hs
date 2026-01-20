module Resolvable.Unification
  ( Unifier(..)
  , unify
  , unifies
  , matchHead
  , extendUnifier ) where

import Control.Monad ((<=<))
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Maybe (isNothing)

import Resolvable.Prolog
import Util.Fresh

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

data Unifier = Unifier (M.Map Term Term) | NoUnifier

instance Show Unifier where
  show NoUnifier = "no unifier"
  show (Unifier m) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList m)) ++ "}"

identity :: Unifier
identity = Unifier M.empty

extendWithUnifier :: Fresh Unifier -> Unifier -> Fresh Unifier
extendWithUnifier _ NoUnifier = return NoUnifier
extendWithUnifier u1 (Unifier u2) = M.foldrWithKey (\k a -> (updateMapping k a =<<)) u1 u2
  where
    updateMapping :: Term -> Term -> Unifier -> Fresh Unifier
    updateMapping _ _ NoUnifier = return NoUnifier
    updateMapping k v u@(Unifier m) = do
      let v' = M.lookup k m
      maybe (return $ Unifier $ M.insert k v m) extend v'
      where
        extend :: Term -> Fresh Unifier
        extend v' = extendUnifier u v' v

foldUnifiers :: Foldable t => t Unifier -> Fresh Unifier
foldUnifiers = foldl extendWithUnifier (return identity)

unify :: Term -> Term -> Fresh Unifier
unify = extendUnifier identity

unifies :: Term -> Term -> Bool
unifies t1 t2 = case runFresh (unify t1 t2) of
  NoUnifier -> False
  Unifier _ -> True

matchHead :: Term -> Rule a -> Bool
matchHead t = unifies t . rhead

unifyList :: Unifier -> [Term] -> [Term] -> Fresh Unifier
unifyList u t1 t2 = do
  maybe (return NoUnifier) (foldUnifiers <=< sequence) (zipWithStrict (extendUnifier u) t1 t2)

extendUnifier :: Unifier -> Term -> Term -> Fresh Unifier
extendUnifier NoUnifier _ _ = return NoUnifier
extendUnifier u TWildcard t = do
  v <- TVar <$> fresh "_X"
  extendUnifier u v t
extendUnifier u v@(TVar _) t
  | t == v = return u
  | otherwise   = return $ Unifier (M.singleton v t)
extendUnifier u (TOp op1 args1) (TOp op2 args2)
  | op1 == op2 = unifyList u args1 args2
  | otherwise  = return NoUnifier
extendUnifier _ (TOp _ _) _ = return NoUnifier
extendUnifier u (TAtomQuoted a) (TAtomQuoted b)
  | a == b    = return u
  | otherwise = return NoUnifier
extendUnifier _ (TAtomQuoted _) _ = return NoUnifier
extendUnifier u (TAtom a) (TAtom b)
  | a == b    = return u
  | otherwise = return NoUnifier
extendUnifier _ (TAtom _) _ = return NoUnifier
extendUnifier u (TList hs1 t1) (TList hs2 t2) = go u hs1 hs2 t1 t2
  where
    go :: Unifier -> [Term] -> [Term] -> Maybe String -> Maybe String -> Fresh Unifier
    go u' []        hs2'      Nothing t2'
      | null hs2' && isNothing t2' = return u'
      | otherwise = return NoUnifier
    go u' []        hs2'      (Just v) t2' =
      extendUnifier u' (TVar v) (TList hs2' t2')
    go _  _         []        _ _ = return NoUnifier
    go u' (h1:hs1') (h2:hs2') t1' t2' = do
      u'' <- extendUnifier u' h1 h2
      go u'' hs1' hs2' t1' t2'
extendUnifier _ (TList _ _) _ = return NoUnifier
extendUnifier u (TString a) (TString b)
  | a == b    = return u
  | otherwise = return NoUnifier
extendUnifier _ (TString _) _ = return NoUnifier
extendUnifier u (TPred h1 args1) (TPred h2 args2)
  | h1 == h2 = unifyList u args1 args2
  | otherwise = return NoUnifier
extendUnifier _ (TPred _ _) _ = return NoUnifier
