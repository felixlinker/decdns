{-# LANGUAGE ScopedTypeVariables #-}
module Util.PartialOrder
  ( PartialOrder
  , empty
  , toList
  , toAscList
  , fromList
  , rel
  , transitiveClosure
  , invert
  , insert
  , remove
  , filterLefts
  , filterRights ) where

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

transitiveClosure :: forall a. Ord a => PartialOrder a -> PartialOrder a
transitiveClosure (PartialOrder d) = PartialOrder $ uncurry go (M.toList <$> M.partition null d)
  where
    go :: M.Map a (S.Set a) -> [(a, S.Set a)] -> M.Map a (S.Set a)
    go acc els
      | null els = acc
      | otherwise = uncurry go (foldr (uncurry . foldf) (acc, []) els)

    foldf :: (a, S.Set a) -> M.Map a (S.Set a) -> [(a, S.Set a)] -> (M.Map a (S.Set a), [(a, S.Set a)])
    foldf el@(k, vs) closure acc
      | all closed vs =
        let vs' = foldr (\v s -> maybe s (<> s) (M.lookup v closure)) vs vs
        in (M.insert k vs' closure, acc)
      | otherwise = (closure, el:acc)
      where
        closed :: a -> Bool
        closed a = M.member a closure || not (M.member a d)

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
