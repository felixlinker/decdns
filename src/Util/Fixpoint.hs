module Util.Fixpoint
  ( fixpoint ) where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.State as St
import qualified Data.Set as S

type Changed = St.State Bool

fixpoint :: (Foldable t, Ord a) => (S.Set a -> t a) -> S.Set a -> S.Set a
fixpoint addF s = St.evalState (fixpoint' addF s) False

fixpoint' :: (Foldable t, Ord a) => (S.Set a -> t a) -> S.Set a -> Changed (S.Set a)
fixpoint' addF acc = do
  acc' <- foldM (flip checkInsert) acc (addF acc)
  changed <- St.get
  if changed
    then fixpoint' addF acc'
    else return acc'
  where
    checkInsert :: Ord a => a -> S.Set a -> Changed (S.Set a)
    checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)
