{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.NegatedQuery
  ( withouts ) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Map as M

import Resolvable.Dependencies
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core

type NegatedQuery = Term

negateQuery :: ConstrainedQuery -> [NegatedQuery]
negateQuery (CQuery t neqs) = map (`U.apply` t) disjs
  where
    disjs :: [U.Unifier]
    disjs = if null neqs then [U.identity] else mapMaybe (uncurry U.unify) (S.toList neqs)

without :: ConstrainedQuery -> NegatedQuery -> Maybe ConstrainedQuery
without cq@(CQuery t neqs) neg =
  maybe (Just cq) (fmap (CQuery t . (neqs <>)) . unifierToNeqs . restrict) (U.unify t neg)
  where
    dom :: S.Set Term
    dom = S.fromList $ map TVar (vars t)

    -- This handles cases where the negated query is more general than the
    -- constrained query. For example: "[com, a] \ [com, X]" would unify under
    -- "X -> a", but we don't care about that concrete instance.
    restrict :: U.Unifier -> U.Unifier
    restrict (U.Unifier um) = U.Unifier (M.filterWithKey (\k v -> k `S.member` dom || v `S.member` dom) um)

    unifierToNeqs :: U.Unifier -> Maybe CU.Neqs
    unifierToNeqs u@(U.Unifier um)
      | U.isRenaming u = Nothing
      | otherwise = Just $ S.fromAscList $ M.toAscList um

withouts :: ConstrainedQuery -> [ConstrainedQuery] -> Maybe ConstrainedQuery
withouts q (concatMap negateQuery -> negs) = foldr (\nq -> (>>= (`without` nq))) (Just q) negs
