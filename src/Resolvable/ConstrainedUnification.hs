module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier(..)
  , Neqs
  , identity
  , apply
  , addConstraint
  , extendUnifier
  , mergeUnifiers ) where

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Resolvable.Unification as U
import Resolvable.Prolog
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(bimap))
import Util.List (zipStrict)

type Neqs = S.Set (Term, Term)

data ConstrainedUnifier =
  CUnifier  { unif :: U.Unifier
            , constraints :: Neqs }

instance Show ConstrainedUnifier where
  show (CUnifier u neqs) = show u ++ " s.t. " ++ intercalate ", " (map (\(a, b) -> show a ++ " \\= " ++ show b) $ S.toList neqs)

identity :: ConstrainedUnifier
identity = CUnifier U.identity S.empty

apply :: ConstrainedUnifier -> Term -> Term
apply (CUnifier u _) = U.apply u

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
simplifyNeq a b = case (a, b) of
  (TVar _, _) -> [(a, b)]
  (_, TVar _) -> [(a, b)]
  (TWildcard, _) -> [(a, b)] -- preserve contradiction
  (_, TWildcard) -> [(a, b)] -- preserve contradiction
  (TOp opa argsa, TOp opb argsb)
    | opa == opb -> maybe [] rec (zipStrict argsa argsb)
    | otherwise -> []
  (TAtomQuoted ata, TAtomQuoted atb)
    | ata /= atb -> []
    -- otherwise: preserve contradiction
  (TAtom ata, TAtom atb)
    | ata /= atb -> []
    -- otherwise: preserve contradiction
  (TList [] Nothing, TList (_:_) _) -> []
  (TList (_:_) _, TList [] Nothing) -> []
  -- TODO: Implement more reasoning on lists
  (TList _ _, TList _ _) -> [(a, b)]
  (TString stra, TString strb)
    | stra /= strb -> []
    -- otherwise: preserve contradiction
  (TPred pa argsa, TPred pb argsb)
    | pa == pb -> maybe [] rec (zipStrict argsa argsb)
    | otherwise -> []
  _ -> []
  where
    rec :: [(Term, Term)] -> [(Term, Term)]
    rec = concatMap (uncurry simplifyNeq)
