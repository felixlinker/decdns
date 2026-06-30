module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier(..)
  , Neqs
  , identity
  , fromUnifier
  , apply
  , applyRule
  , addConstraint
  , extendUnifier
  , mergeUnifiers ) where

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(bimap))
import Util.List (zipStrict)

type Neqs = S.Set (Term, Term)

data ConstrainedUnifier =
  CUnifier  { unif :: U.Unifier
            , constraints :: Neqs }

instance Show ConstrainedUnifier where
  show (CUnifier u neqs) =
    let cond = intercalate ", " (map (\(a, b) -> show a ++ " \\= " ++ show b) $ S.toList neqs)
    in  show u ++ " s.t. " ++ (if null cond then "True" else cond)

identity :: ConstrainedUnifier
identity = CUnifier U.identity S.empty

fromUnifier :: U.Unifier -> ConstrainedUnifier
fromUnifier = (`CUnifier` S.empty)

apply :: ConstrainedUnifier -> Term -> Term
apply (CUnifier u _) = U.apply u

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

addConstraint :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
addConstraint a b (CUnifier u neqs) =
  let a' = U.apply u a
      b' = U.apply u b
      new = simplifyNeq a' b'
  in  if any (uncurry equiv) new
      then Nothing
      else Just (CUnifier u (foldr S.insert neqs new))

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  let neqs' = S.fromList $ simplify $ map (bimap (U.apply u') (U.apply u')) $ S.toList neqs
  guard (all (uncurry (/=)) neqs')
  return $ CUnifier u' neqs'

mergeUnifiers :: ConstrainedUnifier -> ConstrainedUnifier -> Maybe ConstrainedUnifier
mergeUnifiers keep (CUnifier (U.Unifier m) neqs) =
  let withNeqs = foldr (\(a, b) -> (>>= addConstraint a b)) (Just keep) neqs
  in foldr (\(a, b) -> (>>= extendUnifier a b)) withNeqs (M.toList m)

simplify :: [(Term, Term)] -> [(Term, Term)]
simplify = concatMap (uncurry simplifyNeq)

simplifyNeq :: Term -> Term -> [(Term, Term)]
simplifyNeq a b = case a of
  TVar _ -> preserve
  TWildcard -> preserve
  TFunctor na argsa -> case b of
    TVar _ -> preserve
    TWildcard -> preserve
    TFunctor nb argsb
      | na == nb -> maybe [] rec (zipStrict argsa argsb)
    _ -> []
  TEmptyList -> dropWhenStrictEq TEmptyList
  a'@(TAtomQuoted _) -> dropWhenStrictEq a'
  a'@(TAtom _) -> dropWhenStrictEq a'
  a'@(TString _) -> dropWhenStrictEq a'
  a'@(TInt _) -> dropWhenStrictEq a'
  a'@(TComplex _) -> dropWhenStrictEq a'
  where
    preserve :: [(Term, Term)]
    preserve = [(a, b)]

    dropWhenStrictEq :: Term -> [(Term, Term)]
    dropWhenStrictEq a' = case b of
      TVar _ -> preserve
      TWildcard -> preserve
      b'  | a' == b' -> preserve
          | otherwise -> []

    rec :: [(Term, Term)] -> [(Term, Term)]
    rec [] = [(a, b)]  -- preserve contradiction
    rec zipped = concatMap (uncurry simplifyNeq) zipped
