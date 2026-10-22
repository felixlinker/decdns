{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Disunification
  ( withouts
  , computeRegressions ) where

import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving (getSolved)
import Resolvable.Consequences
import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core

unifySimplified :: Fact -> Fact -> Maybe U.Unifier
unifySimplified f1 f2 = do
  u <- U.unify f1 f2
  _ <- applySimplify u f1
  return u

extendUnif :: U.Unify a => U.Unifier -> a -> a -> Maybe U.Unifier
extendUnif u t1 t2 = U.extendUnifier t1 t2 u

disunify :: Fact -> Fact -> [Fact]
disunify base@(Fact t st) neg@(Fact _ (Store.CStore negNeqs _)) =
  -- If the two facts to not unify, there is no thing to subtract.
  maybe [base] negCases (unifySimplified base neg)
  where
    -- If they unify, there are two cases. Either, the heads don't unify, or
    -- the heads unify but one of the constraints is false.
    negCases :: U.Unifier -> [Fact]
    negCases u = notUnified u ++ notConstraints u

    addNeq :: (Term, Term) -> Maybe Store.ConstraintStore
    addNeq (a, b) = getSolved . Store.simplify =<< Store.add st (a \= b)

    -- We describe when the heads don't unify by negating the unifier as it
    -- applies to @t@ (i.e., restricting it).
    notUnified :: U.Unifier -> [Fact]
    notUnified u =
      let U.Unifier (M.toList -> toNegate) = U.filterRenamings $ U.restrict (vars t) u
      in mapMaybe (fmap (Fact t) . addNeq) toNegate

    -- We negate each subtract constraint only if it is an inequality. This is
    -- because all other constraints would be false for resolvable queries.
    notConstraints :: U.Unifier -> [Fact]
    notConstraints u = mapMaybe ((`applySimplify` base) <=< uncurry (extendUnif u) . U.apply u) negNeqs

withouts :: Fact -> [Fact] -> [Fact]
withouts q = foldr (\nq -> concatMap (`disunify` nq)) [q]

computeRegressions :: S.Set Fact -> S.Set Fact -> S.Set Fact
computeRegressions (S.toList -> resOld) (S.toList -> resNew) =
  S.fromList $ concatMap (`withouts` resNew) resOld
