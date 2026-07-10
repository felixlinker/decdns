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
import Data.Maybe (mapMaybe)

toExcept :: Monad m => m a -> ExceptT e m a
toExcept = ExceptT . fmap Right

computeRegressions :: Fresh (S.Set (Rule (Maybe RComp))) -> Fresh (S.Set (Rule (Maybe RComp))) -> ExceptT String Fresh [ConstrainedQuery]
computeRegressions (toExcept -> oldf) (toExcept -> newf) = do
  oldRs <- oldf
  newRs <- newf
  -- TODO: Guard that nothing is dropped
  resOld <- mapMaybe queryFromRule . S.toList <$> computeResolvable oldRs
  resNew <- mapMaybe queryFromRule . S.toList <$> computeResolvable newRs
  return $ concatMap (`withouts` resNew) resOld
