module Util.Set
  ( mapMonotonicF ) where

import qualified Data.Set as S

mapMonotonicF :: (Applicative f, Ord b) => (a -> f b) -> S.Set a -> f (S.Set b)
mapMonotonicF f = fmap S.fromAscList . foldr (\a acc -> (:) <$> f a <*> acc) (pure []) . S.toAscList
