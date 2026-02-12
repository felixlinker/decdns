{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Dependencies
  ( computeRegressions ) where

import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as S

import DNS.Name
import Resolvable.Dependencies
import Resolvable.Prolog
import Util.Fresh
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Data.Bifunctor (Bifunctor(bimap))

computeRegressions :: [Name String] -> Fresh [Rule a] -> Fresh [Rule a] -> Fresh [ConstrainedQuery]
computeRegressions rootServers oldf newf = do
  (old, new) <- (,) <$> c oldf <*> c newf
  return $ do  -- Enter list monad
      q <- old
      let mappings = map (matchDelta q) new
      if all isNothing mappings
        then [q]
        else concatMap (fromMaybe []) mappings
  where
    c :: Fresh [Rule a] -> Fresh [ConstrainedQuery]
    c = computeResolvable rootServers

    biapply :: U.Unifier -> (Term, Term) -> (Term, Term)
    biapply u = bimap (U.apply u) (U.apply u)

    matchDelta :: ConstrainedQuery -> ConstrainedQuery -> Maybe [ConstrainedQuery]
    matchDelta cq@(CQuery q _) (CQuery q' (S.toList -> neqs')) = do
        u <- U.unify q q'
        return $ mapMaybe (addEq u cq) neqs'

    addEq :: U.Unifier -> ConstrainedQuery -> (Term, Term) -> Maybe ConstrainedQuery
    addEq u (CQuery q (S.map (biapply u) -> neqs)) (biapply u -> (a, b)) = do
      CU.CUnifier u' neqs' <- CU.extendUnifier a b (CU.CUnifier u neqs)
      return $ CQuery (U.apply u' q) neqs'
