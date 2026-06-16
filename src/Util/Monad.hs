module Util.Monad
  ( one
  , foreachM
  , exceptMaybe
  , abortOnError
  , handleError ) where
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Control.Monad (MonadPlus (mzero))

one :: Functor f => f a -> f [a]
one = fmap return

foreachM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
foreachM t f = concat <$> mapM f t

exceptMaybe :: e -> Maybe a -> Except e a
exceptMaybe e = maybe (throwE e) return

abortOnError :: MonadPlus m => Except e a -> m a
abortOnError = either (const mzero) return . runExcept

handleError :: (Show e) => (a -> IO ()) -> Except e a -> IO ()
handleError hnd = either print hnd . runExcept
