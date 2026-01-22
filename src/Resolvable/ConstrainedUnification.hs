module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier
  , identity
  , apply
  , addConstraint
  , extendUnifier ) where

import qualified Data.Set as S

import qualified Resolvable.Unification as U
import Resolvable.Prolog
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(bimap))

type Neqs = S.Set (Term, Term)

data ConstrainedUnifier = CUnifier U.Unifier Neqs

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
  in if a' == b' then Nothing else Just (CUnifier u (S.insert (a', b') neqs))

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  let neqs' = S.map (bimap (U.apply u') (U.apply u')) neqs
  guard (all (uncurry (/=)) (S.toList neqs'))
  return $ CUnifier u' neqs'
