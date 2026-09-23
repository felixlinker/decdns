module Util.Set
  ( mapMonotonicF
  , checkInsert ) where

import Control.Monad.Trans.State as St

import qualified Data.Set as S

import Util.Fixpoint

mapMonotonicF :: (Applicative f, Ord b) => (a -> f b) -> S.Set a -> f (S.Set b)
mapMonotonicF f = fmap S.fromAscList . foldr (\a acc -> (:) <$> f a <*> acc) (pure []) . S.toAscList

checkInsert :: Ord a => a -> S.Set a -> Changed (S.Set a)
checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)
