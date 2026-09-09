{-# LANGUAGE ScopedTypeVariables #-}
module Util.Fixpoint
  ( Changed
  , changed
  , unchanged
  , fixpoint
  , fixpointSet ) where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.State as St
import qualified Data.Set as S

type Changed = St.State Bool

changed :: a -> Changed a
changed a = St.put True >> return a

unchanged :: a -> Changed a
unchanged = return

fixpoint :: forall a. (a -> Changed a) -> a -> a
fixpoint f v = St.evalState (aux v) False
  where
    aux :: a -> Changed a
    aux acc = do
      St.put False
      acc' <- f acc
      hasChanged <- St.get
      if hasChanged
        then aux acc'
        else return acc'

fixpointSet :: (Foldable t, Ord a) => (S.Set a -> t a) -> S.Set a -> S.Set a
fixpointSet newF = fixpoint (\acc -> foldM (flip checkInsert) acc (newF acc))
  where
    checkInsert :: Ord a => a -> S.Set a -> Changed (S.Set a)
    checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)
