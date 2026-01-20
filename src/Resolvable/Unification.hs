module Resolvable.Unification
  ( Unifier(..)
  , apply
  , unify
  , unifies
  , matchHead
  , extendUnifier ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)

import Resolvable.Prolog

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

zipStrict :: [a] -> [b] -> Maybe [(a, b)]
zipStrict = zipWithStrict (,)

data Unifier = Unifier (M.Map Term Term) | NoUnifier

instance Show Unifier where
  show NoUnifier = "no unifier"
  show (Unifier m) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList m)) ++ "}"

identity :: Unifier
identity = Unifier M.empty

singleton :: Term -> Term -> Unifier
singleton v@(TVar _) t = Unifier (M.singleton v t)
singleton _ _ = undefined

apply :: Unifier -> Term -> Maybe Term
apply NoUnifier _ = Nothing
apply u (TOp op args) = TOp op <$> mapM (apply u) args
apply (Unifier m) v@(TVar _) = Just $ fromMaybe v (M.lookup v m)
apply _ TWildcard = Just TWildcard
apply _ t@(TAtomQuoted _) = Just t
apply _ t@(TAtom _) = Just t
apply u (TList ts mt) = do
  listApplied <- mapM (apply u) ts
  (tailApplied, tailVarApplied) <- applyTailVar mt
  return $ TList (listApplied ++ tailApplied) tailVarApplied
  where
    applyTailVar :: Maybe Term -> Maybe ([Term], Maybe Term)
    applyTailVar Nothing = Just ([], Nothing)
    applyTailVar (Just tailTerm) = do
      tailApplied <- apply u tailTerm
      case tailApplied of
        TList ts' t -> return (ts', t)
        t@(TVar _) -> return ([], Just t)
        _ -> Nothing
apply _ t@(TString _) = Just t
apply u (TPred n ts) = TPred n <$> mapM (apply u) ts

unify :: Term -> Term -> Unifier
unify = extendUnifier identity

unifies :: Term -> Term -> Bool
unifies t1 t2 = case unify t1 t2 of
  NoUnifier -> False
  Unifier _ -> True

matchHead :: Term -> Rule a -> Bool
matchHead t = unifies t . rhead

extendUnifier :: Unifier -> Term -> Term -> Unifier
extendUnifier NoUnifier _ _ = NoUnifier
extendUnifier u@(Unifier m) from to =
  -- Call @apply u to@ so that the unifier stays normal
  case apply u to of
    Nothing -> NoUnifier
    Just t -> unif from t
  where
    unifList :: [Term] -> [Term] -> Unifier
    unifList l1 l2 = maybe NoUnifier (foldr (\(t1, t2) bu -> extendUnifier bu t1 t2) u) (zipStrict l1 l2)

    unif :: Term -> Term -> Unifier
    unif TWildcard _ = u
    unif v@(TVar _) t
      | t == v = u
      | otherwise   = maybe base rec (M.lookup v m)
        where
          base :: Unifier
          base =  let applied = traverse (apply (singleton v t)) m
                  in maybe NoUnifier (Unifier . M.insert v t) applied

          rec :: Term -> Unifier
          rec substd = extendUnifier u substd t
    unif (TOp op1 args1) (TOp op2 args2)
      | op1 == op2 = unifList args1 args2
      | otherwise  = NoUnifier
    unif (TOp _ _) _ = NoUnifier
    unif (TAtomQuoted a) (TAtomQuoted b)
      | a == b    = u
      | otherwise = NoUnifier
    unif (TAtomQuoted _) _ = NoUnifier
    unif (TAtom a) (TAtom b)
      | a == b    = u
      | otherwise = NoUnifier
    unif (TAtom _) _ = NoUnifier
    unif (TList hs1 t1) (TList hs2 t2) = go' u hs1 hs2 t1 t2
      where
        go' :: Unifier -> [Term] -> [Term] -> Maybe Term -> Maybe Term -> Unifier
        go' u' []        hs2'      Nothing t2'
          | null hs2' && isNothing t2' = u'
          | otherwise = NoUnifier
        go' u' []        hs2'      (Just v) t2' =
          extendUnifier u' v (TList hs2' t2')
        go' _  _         []        _ _ = NoUnifier
        go' u' (h1:hs1') (h2:hs2') t1' t2' =
          go' (extendUnifier u' h1 h2) hs1' hs2' t1' t2'
    unif (TList _ _) _ = NoUnifier
    unif (TString a) (TString b)
      | a == b    = u
      | otherwise = NoUnifier
    unif (TString _) _ = NoUnifier
    unif (TPred h1 args1) (TPred h2 args2)
      | h1 == h2 = unifList args1 args2
      | otherwise = NoUnifier
    unif (TPred _ _) _ = NoUnifier
