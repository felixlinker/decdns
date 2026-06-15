{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Util.PartialOrder
  ( PartialOrder
  , empty
  , Util.PartialOrder.null
  , toList
  , toFlatList
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
import Data.Bifunctor (Bifunctor(first))
import Data.Tuple (swap)

-- po maps a key to all elements that are smaller than the key
newtype PartialOrder a = PartialOrder { toSmaller :: M.Map a (S.Set a) }

empty :: PartialOrder a
empty = PartialOrder M.empty

null :: PartialOrder a -> Bool
null = M.null . toSmaller

toList :: PartialOrder a -> [(S.Set a, a)]
toList = toAscList

toFlatList :: PartialOrder a -> [(a, a)]
toFlatList = concatMap (\(as, a) -> map (,a) (S.toList as)) . toList

toAscList :: PartialOrder a -> [(S.Set a, a)]
toAscList = map swap . M.toAscList . toSmaller

fromList :: Ord a => [(a, a)] -> PartialOrder a
fromList = foldr (uncurry insert . first S.singleton) empty

rel :: Ord a => a -> PartialOrder a -> S.Set a
rel a = fromMaybe S.empty . M.lookup a . toSmaller

minima :: Ord a => PartialOrder a -> S.Set a
minima = S.fromAscList . map fst . filter (S.null . snd) . M.toAscList . toSmaller

transitiveClosure :: forall a. Ord a => PartialOrder a -> Maybe (PartialOrder a)
transitiveClosure (PartialOrder d) =
  let (mins, recArg) = M.partition S.null d
  in PartialOrder <$> rec (M.toList recArg) mins
  where
    rec :: [(a, S.Set a)] -> M.Map a (S.Set a) -> Maybe (M.Map a (S.Set a))
    rec [] acc = Just acc
    rec ls acc =
      let (ready, recArg) = partition (all closed . snd) ls
      in if Prelude.null ready
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
    foldF oldK oldVs d = foldr (insert (S.singleton oldK)) d oldVs

insert :: Ord a => S.Set a -> a -> PartialOrder a -> PartialOrder a
insert vs k (PartialOrder m) = PartialOrder (M.alter (Just . maybe vs (vs <>)) k m)

remove :: Ord a => a -> PartialOrder a -> PartialOrder a
remove key (PartialOrder m) = PartialOrder $ M.map (S.delete key) $ M.delete key m

filterLefts :: (a -> Bool) -> PartialOrder a -> PartialOrder a
filterLefts p = PartialOrder . M.map (S.filter p) . toSmaller

filterRights :: (a -> Bool) -> PartialOrder a -> PartialOrder a
filterRights p = PartialOrder . M.filterWithKey (\k _ -> p k) . toSmaller
