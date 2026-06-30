module Util.Monad
  ( one
  , foreachM
  , exceptMaybe
  , maybeExcept ) where

import Control.Monad.Trans.Except (Except, ExceptT, throwE, runExcept)

one :: Functor f => f a -> f [a]
one = fmap return

foreachM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
foreachM t f = concat <$> mapM f t

exceptMaybe :: Monad m => e -> Maybe a -> ExceptT e m a
exceptMaybe e = maybe (throwE e) return

maybeExcept :: Except e a -> Maybe a
maybeExcept = either (const Nothing) Just . runExcept
