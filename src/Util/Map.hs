module Util.Map
  ( groupBy
  , alterDefF
  , add ) where

import qualified Data.Map as M

groupBy :: (Ord k, Foldable t) => (a -> k) -> t a -> M.Map k [a]
groupBy f = foldr (\a -> M.alter (Just . maybe [a] (a:)) (f a)) M.empty

whenDefined :: Applicative f => (a -> f a) -> Maybe a -> f (Maybe a)
whenDefined f = maybe (pure Nothing) (fmap Just . f)

alterDefF :: (Applicative f, Ord k) => (a -> f a) -> k -> M.Map k a -> f (M.Map k a)
alterDefF f = M.alterF (whenDefined f)

add :: (Semigroup s, Ord k) => k -> s -> M.Map k s -> M.Map k s
add = M.insertWith (<>)
