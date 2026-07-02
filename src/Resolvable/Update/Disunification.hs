{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Disunification
  ( withouts ) where

import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M

import Resolvable.Dependencies
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core

negateQuery :: ConstrainedQuery -> Maybe [Term]
negateQuery (CQuery t (CU.toList -> neqs))
  | null neqs = Nothing
  -- Treat failing unification as undefined because we know that CU.toList will
  -- return variables as left term, which always succeeds.
  | otherwise = Just $ map (maybe undefined (`U.apply` t) . uncurry U.unify) neqs

-- @unified a b@ "intersects" a with b.
unified :: ConstrainedQuery -> Term -> Maybe ConstrainedQuery
unified (CQuery t neqs) t' = do
  u <- U.unify t t'
  let app = U.apply u
  neqs' <- CU.fromList $ app $ CU.toList neqs
  return $ CQuery (U.apply u t) neqs'

-- @disunified a b@ "subtracts" b from a. Assumes that intersection of b and a
-- is non-empty.
disunified :: ConstrainedQuery -> Term -> [ConstrainedQuery]
disunified (CQuery t (CU.toList -> neqs)) t' = fromMaybe [] $ do
  (U.Unifier (M.toList -> u)) <- U.filterRenamings . U.restrict (vars t) <$> U.unify t t'
  -- The above unifier is the intersection of @t@ and @t'@. As we want to
  -- subtract @t'@, we treat the the unifier as a conjuction of assignments, the
  -- negation of which is a disjunction of inequalites, which we add to the
  -- constrained query below.
  return $ mapMaybe (fmap (CQuery t) . CU.fromList . (:neqs)) u

without :: ConstrainedQuery -> ConstrainedQuery -> [ConstrainedQuery]
without pos@(CQuery t _) neg@(CQuery t' _)
  | U.unifies t t' =
            -- If @neg@ does not contain inequalities, we subtract @t'@ from
            -- @pos@ by disunification.
      maybe (disunified pos t')
            -- If @neg@ contains inequalities, we can easliy create its inverse
            -- by treating the inequalites as disjunction of assignments. In
            -- that case, we only need to unify @pos@ with each disjunct.
            (mapMaybe (unified pos))
            (negateQuery neg)
  -- If the intersection of t and t' is empty, we need not consider t' for
  -- subtraction.
  | otherwise = [pos]

withouts :: ConstrainedQuery -> [ConstrainedQuery] -> [ConstrainedQuery]
withouts q = foldr (\nq -> concatMap (`without` nq)) [q]
