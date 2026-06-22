module Util.Monad
  ( one
  , foreachM
  , exceptMaybe
  , maybeExcept
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

maybeExcept :: Except e a -> Maybe a
maybeExcept = either (const Nothing) Just . runExcept

abortOnError :: MonadPlus m => Except e a -> m a
abortOnError = either (const mzero) return . runExcept

handleError :: (a -> IO ()) -> Except String a -> IO ()
handleError hnd = either putStrLn hnd . runExcept
