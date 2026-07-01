module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier(..)
  , Neqs
  , simplify
  , identity
  , fromUnifier
  , apply
  , applyRule
  , addConstraint
  , extendUnifier
  , mergeUnifiers ) where

import Data.Functor (($>))
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
  in CUnifier u . foldr S.insert neqs <$> simplifyNeq a' b'

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  CUnifier u' . S.fromList <$> simplifyL (map (bimap (U.apply u') (U.apply u')) $ S.toList neqs)

mergeUnifiers :: ConstrainedUnifier -> ConstrainedUnifier -> Maybe ConstrainedUnifier
mergeUnifiers keep (CUnifier (U.Unifier m) neqs) =
  let withNeqs = foldr (\(a, b) -> (>>= addConstraint a b)) (Just keep) neqs
  in foldr (\(a, b) -> (>>= extendUnifier a b)) withNeqs (M.toList m)

simplify :: Neqs -> Maybe Neqs
simplify = fmap S.fromAscList . simplifyL . S.toAscList

simplifyL :: [(Term, Term)] -> Maybe [(Term, Term)]
simplifyL = fmap concat . mapM (uncurry simplifyNeq)

simplifyNeq :: Term -> Term -> Maybe [(Term, Term)]
simplifyNeq TWildcard _ = Just []
simplifyNeq _ TWildcard = Just []
simplifyNeq a@(TVar _) b = guard (a /= b) $> [(a, b)]
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

    contradictionWhenEq :: Maybe [(Term, Term)]
    contradictionWhenEq = case b of
      b'  | a == b'   -> Nothing
          | otherwise -> redundant

    rec :: [(Term, Term)] -> Maybe [(Term, Term)]
    rec [] = Nothing
    rec zipped = simplifyL zipped
