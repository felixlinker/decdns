{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.NegatedQuery
  ( withouts ) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Resolvable.Dependencies
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core
import Control.Monad (guard)

-- Take a constrained unifier and negate it. Effectively, all inequalities
-- become assignemnts, and all assignments (in the unifier) become inequalities.
-- The only exception are cases where variables renamed, in the unifier, which
-- are preserved.
negateUnif :: CU.ConstrainedUnifier -> [CU.ConstrainedUnifier]
negateUnif (CU.CUnifier u (CU.toList -> neqs)) =
  let disj = if null neqs
        then [U.identity]
        else foldr (\(t1, t2) -> maybe id (:) (U.unify t1 t2)) [] neqs
  in mapMaybe (`toCUnif` U.toList u) disj
  where
    toCUnif :: U.Unifier -> [(Term, Term)] -> Maybe CU.ConstrainedUnifier
    toCUnif unif = foldr (\ts -> (>>= addUnif ts)) (Just (CU.fromUnifier unif))

    addUnif :: (Term, Term) -> CU.ConstrainedUnifier -> Maybe CU.ConstrainedUnifier
    addUnif (a, b)
      | isVar a && isVar b =  CU.extendUnifier a b
      | otherwise = CU.addConstraint a b

without :: ConstrainedQuery -> ConstrainedQuery -> [ConstrainedQuery]
without pos@(CQuery t _) neg =
  -- First intersect the two sets of terms...
  let intersection = unifyQueries pos neg
  -- ...then apply the resulting unifier, but negated.
  in maybe [pos] (mapMaybe app . negateUnif) intersection
  where
    dom :: S.Set Term
    dom = vars t

    app :: CU.ConstrainedUnifier -> Maybe ConstrainedQuery
    app (CU.restrict dom -> CU.CUnifier u neqs) = do
      (CQuery q neqs') <- U.apply u (Just pos)
      let restricted = CQuery q (CU.union neqs neqs')
      -- If applying the negated unifier yiels the original term, no reduction
      -- occurred. In this case, we must subtract the entire unifier, i.e., the
      -- empty set is the result.
      guard (restricted /= pos)
      return restricted

withouts :: ConstrainedQuery -> [ConstrainedQuery] -> [ConstrainedQuery]
withouts q = foldr (\nq -> concatMap (`without` nq)) [q]
