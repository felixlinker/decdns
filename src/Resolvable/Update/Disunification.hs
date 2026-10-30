{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Disunification
  ( withouts
  , computeRegressions ) where

import Control.Monad (guard)
import qualified Data.Set as S

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Consequences
import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core

unifySimplified :: Fact -> Fact -> Maybe U.Unifier
unifySimplified f1 f2 = do
  u <- U.unify f1 f2
  guard $ not $ null $ applySimplify u f1
  return u

extendUnif :: U.Unify a => U.Unifier -> a -> a -> Maybe U.Unifier
extendUnif u t1 t2 = U.extendUnifier t1 t2 u

disunify :: Fact -> Fact -> [Fact]
disunify base@(Fact t st) neg@(Fact t' (Store.CStore negNeqs negCs)) =
  -- If the two facts to not unify, there is no thing to subtract.
  maybe [base] negCases (unifySimplified base neg)
  where
    -- If they unify, there are two cases. Either, the heads don't unify, or
    -- the heads unify but one of the constraints is false.
    negCases :: U.Unifier -> [Fact]
    negCases u = notUnified u ++ notConstraints u

    notUnified :: U.Unifier -> [Fact]
    notUnified _ = maybe undefined (map (Fact t) . Store.simplify) (Store.add st (noUnify t t'))

    notConstraints :: U.Unifier -> [Fact]
    notConstraints u =
          concatMap (maybe [] (`applySimplify` base) . uncurry (extendUnif u) . U.apply u) negNeqs
      ++  concatMap (maybe [] (applySimplify u . Fact t) . Store.add st . nott) negCs

withouts :: Fact -> [Fact] -> [Fact]
withouts q = foldr (\nq -> concatMap (`disunify` nq)) [q]

computeRegressions :: S.Set Fact -> S.Set Fact -> S.Set Fact
computeRegressions (S.toList -> resOld) (S.toList -> resNew) =
  S.fromList $ concatMap (`withouts` resNew) resOld
