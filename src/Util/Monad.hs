module Util.Monad
  ( one
  , foreachM
  , exceptMaybe
  , maybeExcept
  , failLeft
  , failLeftS
  , handleError
  , handleErrorS ) where

import Control.Monad.Trans.Except (Except, ExceptT, throwE, runExcept)

one :: Functor f => f a -> f [a]
one = fmap return

foreachM :: (Monad m, Monoid b) => [a] -> (a -> m b) -> m b
foreachM t f = mconcat <$> mapM f t

exceptMaybe :: Monad m => e -> Maybe a -> ExceptT e m a
exceptMaybe e = maybe (throwE e) return

maybeExcept :: Except e a -> Maybe a
maybeExcept = either (const Nothing) Just . runExcept

failLeft :: (MonadFail m, Show e) => Either e a -> m a
failLeft = either (fail . show) return

failLeftS :: (MonadFail m) => Either String a -> m a
failLeftS = either fail return

handleError :: (MonadFail m, Show e) => Except e a -> m a
handleError = failLeft . runExcept

handleErrorS :: (MonadFail m) => Except String a -> m a
handleErrorS = failLeftS . runExcept
