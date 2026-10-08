{-# LANGUAGE ScopedTypeVariables #-}
module Util.Fixpoint
  ( ChangedT
  , Changed
  , runChanged
  , onChange
  , changed
  , unchanged
  , fixpointM
  , fixpoint
  , fixpointSetM
  , fixpointSet
  , fixpointP ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import qualified Data.Set as S
import Data.Functor.Identity (Identity(runIdentity))

type ChangedT m = St.StateT Bool m
type Changed = St.State Bool

runChanged :: ChangedT m a -> m (a, Bool)
runChanged = (`St.runStateT` False)

onChange :: Monad m => (a -> b) -> (a -> b) -> ChangedT m a -> m b
onChange whenChanged whenUnchanged change = do
  (a, didChange) <- St.runStateT change False
  return $ (if didChange then whenChanged else whenUnchanged) a

changed :: Monad m => a -> ChangedT m a
changed a = St.put True >> return a

unchanged :: Monad m => a -> ChangedT m a
unchanged = return

fixpointM :: forall a m. Monad m => (a -> ChangedT m a) -> a -> m a
fixpointM f v = St.evalStateT (aux v) False
  where
    aux :: a -> ChangedT m a
    aux acc = do
      St.put False
      acc' <- f acc
      hasChanged <- St.get
      if hasChanged
        then aux acc'
        else return acc'

fixpoint :: (a -> Changed a) -> a -> a
fixpoint f = runIdentity . fixpointM f

fixpointSetM :: (Foldable t, Monad m, Ord a) => (S.Set a -> m (t a)) -> S.Set a -> m (S.Set a)
fixpointSetM newF = fixpointM (\acc -> foldM (flip checkInsert) acc =<< lift (newF acc))
  where
    checkInsert :: (Monad m, Ord a) => a -> S.Set a -> ChangedT m (S.Set a)
    checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)

fixpointSet :: (Foldable t, Ord a) => (S.Set a -> t a) -> S.Set a -> S.Set a
fixpointSet f = runIdentity . fixpointSetM (return . f)

fixpointP :: forall a m. Monad m => (a -> a -> Bool) -> ([a] -> m [a]) -> [a] -> m [a]
fixpointP p supplier = fixpointM (\base -> foldM aux base =<< lift (supplier base))
  where
    aux :: [a] -> a -> ChangedT m [a]
    aux base a =
      if any (p a) base
        then unchanged base
        else changed (a:base)
