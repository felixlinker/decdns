module Util.Fresh
  ( FreshT
  , Fresh
  , runFreshT
  , evalFreshT
  , runFresh
  , evalFresh
  , fresh ) where

import Control.Monad.Trans.State (StateT, evalStateT, get, put, runStateT)
import Data.Functor.Identity (Identity, runIdentity)

-- | State-based transformer that yields fresh variable names.
type FreshT m = StateT Int m

-- | Concrete fresh-name monad without effects.
type Fresh = FreshT Identity

runFreshT :: FreshT m a -> Int -> m (a, Int)
runFreshT = runStateT

evalFreshT :: Monad m => FreshT m a -> Int -> m a
evalFreshT = evalStateT

runFresh :: Fresh a -> (a, Int)
runFresh m = runIdentity (runFreshT m 0)

evalFresh :: Fresh a -> a
evalFresh m = runIdentity (evalFreshT m 0)

fresh :: Monad m => String -> FreshT m String
fresh prefix = do
  n <- get
  put (n + 1)
  return (prefix ++ show n)
