module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier
  , identity
  , apply
  , addConstraint
  , extendUnifier ) where

import qualified Data.Map as M

import qualified Resolvable.Unification as U
import Resolvable.Prolog
import Control.Monad (guard)
import Data.List (intercalate)

type Neqs = M.Map Term Term

data ConstrainedUnifier = CUnifier U.Unifier Neqs

instance Show ConstrainedUnifier where
  show (CUnifier u neqs) = show u ++ " s.t. " ++ intercalate ", " (map (\(a, b) -> show a ++ " \\= " ++ show b) $ M.toList neqs)

identity :: ConstrainedUnifier
identity = CUnifier U.identity M.empty

apply :: ConstrainedUnifier -> Term -> Term
apply (CUnifier u _) = U.apply u

addConstraint :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
addConstraint a b (CUnifier u neqs) =
  let a' = U.apply u a
      b' = U.apply u b
  in if a' == b' then Nothing else Just (CUnifier u (M.insert a' b' neqs))

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  let neqs' = M.map (U.apply u') neqs
  guard (all (uncurry (/=)) (M.toList neqs'))
  return $ CUnifier u' neqs'
