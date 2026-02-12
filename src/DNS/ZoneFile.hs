{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile
  ( emptyZone
  , Zone(..)
  , insert
  , allRecords
  , mergeZones
  , noMatching ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name as N
import DNS.Record
import qualified Util.Tree as T

data Zone a = Zone
  { zoneName :: Name a
  , root :: T.Node a RData }

emptyZone :: Name a -> Zone a
emptyZone org = Zone org T.emptyNode

pathInZone :: Eq a => Name a -> Name a -> Maybe [a]
pathInZone (Name (reverse . NE.toList -> org)) (Name (reverse . NE.toList -> n)) = pathInZone' org n
  where
    pathInZone' :: Eq a => [a] -> [a] -> Maybe [a]
    pathInZone' [] bs = Just bs
    pathInZone' _ [] = Just []
    pathInZone' (a:as) (b:bs)
      | a == b = pathInZone' as bs
      | otherwise = Nothing

insert :: Ord a => Zone a -> Record (Name a) -> Maybe (Zone a)
insert (Zone norg zroot) (Record n d) = do
  p <- pathInZone norg n
  return $ Zone norg (T.insert p d zroot)

allRecords :: Zone a -> [Record (Name a)]
allRecords (Zone orig ns) = go orig ns
  where
    go :: Name a -> T.Node a RData -> [Record (Name a)]
    go n (T.Node rs ch) =
      map (Record n) rs ++ concat (M.mapWithKey (\l -> go (l <| n)) ch)

mergeZones :: (Ord a, Monoid a) => [Zone a] -> Maybe (Zone a)
mergeZones (concatMap allRecords -> zs) = foldr go (Just (emptyZone N.root)) zs
  where
    go :: Ord a => Record (Name a) -> Maybe (Zone a) -> Maybe (Zone a)
    go r = ((`insert` r) =<<)

noMatching :: Ord a => Record (Name a) -> Zone a -> Bool
noMatching (Record n d) (Zone org z) = maybe True (`go` z) (pathInZone org n)
  where
    go :: Ord a => [a] -> T.Node a RData -> Bool
    go [] (T.Node is _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)
