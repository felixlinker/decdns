{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Dependencies
  ( computeRegressions ) where

import qualified Data.Set as S
import Control.Monad.Trans.Except

import Resolvable.Dependencies
import Resolvable.Prolog.Core
import Util.Fresh
import Resolvable.Resolvability (RComp)
import Resolvable.Update.Disunification (withouts)

toExcept :: Monad m => m a -> ExceptT e m a
toExcept = ExceptT . fmap Right

computeRegressions :: Fresh [Rule (Maybe RComp)] -> Fresh [Rule (Maybe RComp)] -> ExceptT String Fresh [ConstrainedQuery]
computeRegressions (toExcept -> oldf) (toExcept -> newf) = do
  oldRs <- oldf
  newRs <- newf
  resOld <- S.toList <$> computeResolvable oldRs
  resNew <- S.toList <$> computeResolvable newRs
  return $ concatMap (`withouts` resNew) resOld
