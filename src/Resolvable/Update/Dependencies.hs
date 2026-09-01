{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Dependencies
  ( computeRegressions ) where

import qualified Data.Set as S
import Control.Monad.Trans.Except

import Resolvable.Dependencies
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Util.Fresh
import Resolvable.Resolvability (RComp)
import Resolvable.Update.Disunification (withouts)

toExcept :: Monad m => m a -> ExceptT e m a
toExcept = ExceptT . fmap Right

computeRegressions :: Fresh (S.Set (Rule (Maybe RComp))) -> Fresh (S.Set (Rule (Maybe RComp))) -> ExceptT String Fresh [ConstrainedQuery]
computeRegressions (toExcept -> oldf) (toExcept -> newf) = do
  oldRs <- oldf
  newRs <- newf
  resOld <- traverse queryFromRule $ S.toList $ computeResolvable True oldRs
  resNew <- traverse queryFromRule $ S.toList $ computeResolvable True newRs
  return $ concatMap ((`withouts` map resolvableQuery resNew) . resolvableQuery) resOld

-- TODO: Restrict neqs to variables that occur in new head term
resolvableQuery :: ConstrainedQuery -> ConstrainedQuery
resolvableQuery (CQuery t neqs) = CQuery (resolvableFromResolves t) neqs

resolvableFromResolves :: Term -> Term
resolvableFromResolves (toView -> TVResolves r) =
  resolvable
    (resolvesNS r)
    (resolvesQType r)
    (resolvesQName r)
    (resolvesVerify r)
    (resolvesAD r)
resolvableFromResolves t = t
