module Util.PartialOrder
  ( PartialOrder
  , empty
  , toList
  , toAscList
  , fromList
  , rel
  , maxima
  , minima
  , invert
  , insert
  , remove ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

newtype PartialOrder a = PartialOrder { po :: M.Map a (S.Set a) }

empty :: PartialOrder a
empty = PartialOrder M.empty

toList :: PartialOrder a -> [(a, S.Set a)]
toList = M.toList . po

toAscList :: PartialOrder a -> [(a, S.Set a)]
toAscList = M.toAscList . po

fromList :: Ord a => [(a, a)] -> PartialOrder a
fromList = foldr (uncurry insert . fmap S.singleton) empty

rel :: Ord a => a -> PartialOrder a -> S.Set a
rel a = fromMaybe S.empty . M.lookup a . po

maxima :: Ord a => PartialOrder a -> S.Set a
maxima (PartialOrder d) = S.fromAscList $ map fst $ filter (null . snd) $ M.toAscList d

minima :: Ord a => PartialOrder a -> S.Set a
minima (PartialOrder d) = S.filter (not . (`S.member` img)) (M.keysSet d)
  where
    img = S.unions $ M.elems d

invert :: Ord a => PartialOrder a -> PartialOrder a
invert (PartialOrder m) = foldr (uncurry foldF) empty (M.toList m)
  where
    foldF :: Ord a => a -> S.Set a -> PartialOrder a -> PartialOrder a
    foldF oldK oldVs d = foldr (`insert` S.singleton oldK) d oldVs

insert :: Ord a => a -> S.Set a -> PartialOrder a -> PartialOrder a
insert k vs (PartialOrder m) = PartialOrder (M.alter (Just . maybe vs (vs <>)) k m)

remove :: Ord a => a -> PartialOrder a -> PartialOrder a
remove key (PartialOrder m) = PartialOrder $ M.map (S.delete key) $ M.delete key m
