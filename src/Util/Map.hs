module Util.Map
  ( groupByWith
  , groupBy
  , alterDefF
  , add ) where

import qualified Data.Map as M

groupByWith :: (Ord k, Foldable t) => (a -> (k, b)) -> t a -> M.Map k [b]
groupByWith f = foldr (\a -> let (k, v) = f a in M.alter (Just . maybe [v] (v:)) k) M.empty

groupBy :: (Ord k, Foldable t) => (a -> k) -> t a -> M.Map k [a]
groupBy f = groupByWith (\a -> (f a, a))

whenDefined :: Applicative f => (a -> f a) -> Maybe a -> f (Maybe a)
whenDefined f = maybe (pure Nothing) (fmap Just . f)

alterDefF :: (Applicative f, Ord k) => (a -> f a) -> k -> M.Map k a -> f (M.Map k a)
alterDefF f = M.alterF (whenDefined f)

add :: (Semigroup s, Ord k) => k -> s -> M.Map k s -> M.Map k s
add = M.insertWith (<>)
