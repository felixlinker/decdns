{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile
  ( emptyZone
  , Path(..)
  , pathInZone
  , AZone(..)
  , Zone
  , insert
  , allData
  , mergeZones
  , noMatching ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name as N
import DNS.Record
import qualified Util.Tree as T

data AZone a b = Zone
  { zoneName :: Name a
  , root :: T.Node a b }

type Zone a = AZone a RData

emptyZone :: Name a -> Zone a
emptyZone org = Zone org T.emptyNode

data Path a = Path
  -- New origin of the zone; longest common ancestor between new and old name
  (Name a)
  -- Path of the old data in the zone
  [a]
  -- Path of the new data in the zone
  [a]

pathInZone :: (Label a, Eq a) => Name a -> Name a -> Path a
pathInZone (Name (NE.reverse -> (orgr NE.:| org))) (Name (NE.reverse -> (nr NE.:| n)))
  | orgr == nr && nr == lempty = pathInZone' (Name $ NE.singleton lempty) org n
  | otherwise = error "at least one name is relative"
  where
    pathInZone' :: Eq a => Name a -> [a] -> [a] -> Path a
    pathInZone' acc [] bs = Path acc [] bs
    pathInZone' acc as [] = Path acc as []
    pathInZone' acc (a:as) (b:bs)
      | a == b = pathInZone' (a <| acc) as bs
      | otherwise = Path acc as bs

mergeZones :: (Label a, Ord a) => Zone a -> Zone a -> Zone a
mergeZones (Zone org1 t1) (Zone org2 t2) = case pathInZone org1 org2 of
  Path org1' p1 p2 -> Zone org1'
    $ T.insertNode p2 t2
    $ T.insertNode p1 t1 T.emptyNode

insert :: (Label a, Ord a) => AZone a b -> Named a b -> AZone a b
insert (Zone norg zroot) (Named n d) = case pathInZone norg n of
  Path norg' zp rp -> Zone norg'
    $ T.insert rp d
    $ T.insertNode zp zroot T.emptyNode

allData :: AZone a b -> [Named a b]
allData (Zone orig ns) = go orig ns
  where
    go :: Name a -> T.Node a b -> [Named a b]
    go n (T.Node d ch) =
      let base = map (Named n) d
          rec = concat $ M.mapWithKey (\l -> go (l <| n)) ch
      in base ++ rec

noMatching :: (Label a, Ord a) => NamedRecord a -> Zone a -> Bool
noMatching (Named n d) (Zone org z) = case pathInZone org n of
  Path _ [] p -> go p z
  _ -> False
  where
    go :: Ord a => [a] -> T.Node a RData -> Bool
    go [] (T.Node is _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)
