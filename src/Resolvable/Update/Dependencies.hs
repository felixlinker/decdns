{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Dependencies
  ( computeRegressions ) where

import qualified Data.Set as S
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except

import Resolvable.Dependencies
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Util.Fresh
import Resolvable.Resolvability (RComp)
import Resolvable.Update.Disunification (withouts)

computeRegressions :: S.Set (Rule (Maybe RComp)) -> S.Set (Rule (Maybe RComp)) -> ExceptT String Fresh (S.Set ConstrainedQuery)
computeRegressions oldRs newRs = do
  resOld <- lift $ computeResolvable True oldRs
  resNew <- lift $ computeResolvable True newRs
  resOldQs <- S.toList <$> foldM toResolvable S.empty resOld
  resNewQs <- S.toList <$> foldM toResolvable S.empty resNew
  return $ S.fromList $ concatMap (`withouts` resNewQs) resOldQs
  where
    toResolvable :: Monad m => S.Set ConstrainedQuery -> Rule a -> ExceptT String m (S.Set ConstrainedQuery)
    toResolvable s r = do
      q <- queryFromRule r
      return $ maybe s (`S.insert` s) (resolvableQuery q)

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
