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
import Data.Maybe (catMaybes)

toExcept :: Monad m => m a -> ExceptT e m a
toExcept = ExceptT . fmap Right

computeRegressions :: Fresh (S.Set (Rule (Maybe RComp))) -> Fresh (S.Set (Rule (Maybe RComp))) -> ExceptT String Fresh [ConstrainedQuery]
computeRegressions (toExcept -> oldf) (toExcept -> newf) = do
  oldRs <- oldf
  newRs <- newf
  resOld <- catMaybes <$> traverse (fmap resolvableQuery . queryFromRule) (S.toList $ computeResolvable True oldRs)
  resNew <- catMaybes <$> traverse (fmap resolvableQuery . queryFromRule) (S.toList $ computeResolvable True newRs)
  return $ concatMap (`withouts` resNew) resOld

-- TODO: Restrict neqs to variables that occur in new head term
resolvableQuery :: ConstrainedQuery -> Maybe ConstrainedQuery
resolvableQuery (CQuery t neqs) = (`CQuery` neqs) <$> resolvableFromResolves t

resolvableFromResolves :: Term -> Maybe Term
resolvableFromResolves (toView -> TVResolves r)
  | resolvesZone r == referralZone = Nothing
  | otherwise = Just $ resolvable
    (resolvesNS r)
    (resolvesQType r)
    (resolvesQName r)
    (resolvesVerify r)
    (resolvesAD r)
resolvableFromResolves _ = Nothing
