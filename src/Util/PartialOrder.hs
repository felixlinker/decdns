{-# LANGUAGE ScopedTypeVariables #-}
module Util.PartialOrder
  ( PartialOrder
  , empty
  , toList
  , toAscList
  , fromList
  , rel
  , minima
  , transitiveClosure
  , invert
  , insert
  , remove
  , filterLefts
  , filterRights ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (partition)

-- po maps a key to all elements that are smaller than the key
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

minima :: PartialOrder a -> [a]
minima = map fst . filter (S.null . snd) . M.toList . po

transitiveClosure :: forall a. Ord a => PartialOrder a -> Maybe (PartialOrder a)
transitiveClosure (PartialOrder d) =
  let (mins, recArg) = M.partition S.null d
  in PartialOrder <$> rec (M.toList recArg) mins
  where
    rec :: [(a, S.Set a)] -> M.Map a (S.Set a) -> Maybe (M.Map a (S.Set a))
    rec [] acc = Just acc
    rec ls acc =
      let (ready, recArg) = partition (all closed . snd) ls
      in if null ready
        then Nothing
        else rec recArg (foldr foldf acc ready)
      where
        closed :: a -> Bool
        closed a = M.member a acc || not (M.member a d)

    foldf :: (a, S.Set a) -> M.Map a (S.Set a) -> M.Map a (S.Set a)
    foldf (k, vs) acc =
      let vs' = foldr (\v s -> maybe s (<> s) (M.lookup v acc)) vs vs
      in M.insert k vs' acc

invert :: Ord a => PartialOrder a -> PartialOrder a
invert (PartialOrder m) = foldr (uncurry foldF) empty (M.toList m)
  where
    foldF :: Ord a => a -> S.Set a -> PartialOrder a -> PartialOrder a
    foldF oldK oldVs d = foldr (`insert` S.singleton oldK) d oldVs

insert :: Ord a => a -> S.Set a -> PartialOrder a -> PartialOrder a
insert k vs (PartialOrder m) = PartialOrder (M.alter (Just . maybe vs (vs <>)) k m)

remove :: Ord a => a -> PartialOrder a -> PartialOrder a
remove key (PartialOrder m) = PartialOrder $ M.map (S.delete key) $ M.delete key m

filterLefts :: (a -> Bool) -> PartialOrder a -> PartialOrder a
filterLefts p = PartialOrder . M.filterWithKey (\k _ -> p k) . po

filterRights :: (a -> Bool) -> PartialOrder a -> PartialOrder a
filterRights p = PartialOrder . M.map (S.filter p) . po
