module Util.Monad
  ( one
  , foreachM
  , exceptUnsafe
  , exceptMaybe
  , maybeExcept
  , failLeft
  , failLeftS
  , handleError
  , handleMaybe
  , handleErrorS
  , firstP
  , first ) where

import Control.Monad.Trans.Except (Except, ExceptT, throwE, runExcept)
import Data.Maybe (fromMaybe)

one :: Functor f => f a -> f [a]
one = fmap return

foreachM :: (Monad m, Monoid b) => [a] -> (a -> m b) -> m b
foreachM t f = mconcat <$> mapM f t

exceptUnsafe :: (a -> Except String b) -> a -> b
exceptUnsafe f = either error id . runExcept . f

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

handleMaybe :: (MonadFail m, Show e) => e -> Maybe a -> m a
handleMaybe e = handleError . exceptMaybe e

handleErrorS :: (MonadFail m) => Except String a -> m a
handleErrorS = failLeftS . runExcept

firstP :: Monad m => (b -> Bool) -> [a] -> (a -> m b) -> m (Maybe b)
firstP _ [] _ = return Nothing
firstP p (h:t) m = do
  b <- m h
  if p b then return (Just b) else firstP p t m

first :: Monad m => [a] -> (a -> m Bool) -> m Bool
first as = fmap (fromMaybe False) . firstP id as
