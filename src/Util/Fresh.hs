module Util.Fresh
  ( FreshT
  , Fresh
  , runFreshT
  , evalFreshT
  , runFresh
  , evalFresh
  , fresh
  , avoiding ) where

import Control.Monad.Trans.State (StateT, evalStateT, get, put, runStateT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Char (isDigit)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | State-based transformer that yields fresh variable names.
type FreshT m = StateT Int m

-- | Concrete fresh-name monad without effects.
type Fresh = FreshT Identity

runFreshT :: FreshT m a -> Int -> m (a, Int)
runFreshT = runStateT

evalFreshT :: Monad m => FreshT m a -> Int -> m a
evalFreshT = evalStateT

runFresh :: Fresh a -> a
runFresh m = fst $ runIdentity (runFreshT m 0)

evalFresh :: Fresh a -> a
evalFresh m = runIdentity (evalFreshT m 0)

fresh :: Monad m => String -> FreshT m String
fresh prefix = do
  n <- get
  put (n + 1)
  return (prefix ++ show n)

avoiding :: (Foldable t, Monad m) => t String -> FreshT m ()
avoiding = put . foldr (\v -> (+ 1) . max (digitTail v)) 0
  where
    digitTail :: String -> Int
    digitTail s = fromMaybe 0 $ do
      let prefix = dropWhileEnd isDigit s
      suffix <- stripPrefix prefix s
      readMaybe suffix
