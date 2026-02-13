{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile
  ( emptyZone
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

pathInZone :: Eq a => Name a -> Name a -> Maybe [a]
pathInZone (Name (reverse . NE.toList -> org)) (Name (reverse . NE.toList -> n)) = pathInZone' org n
  where
    pathInZone' :: Eq a => [a] -> [a] -> Maybe [a]
    pathInZone' [] bs = Just bs
    pathInZone' (_:_) [] = Nothing
    pathInZone' (a:as) (b:bs)
      | a == b = pathInZone' as bs
      | otherwise = Nothing

insert :: Ord a => AZone a b -> Named a b -> Maybe (AZone a b)
insert (Zone norg zroot) (Named n d) = do
  p <- pathInZone norg n
  return $ Zone norg (T.insert p d zroot)

allData :: AZone a b -> [Named a b]
allData (Zone orig ns) = go orig ns
  where
    go :: Name a -> T.Node a b -> [Named a b]
    go n (T.Node d ch) =
      let base = map (Named n) d
          rec = concat $ M.mapWithKey (\l -> go (l <| n)) ch
      in base ++ rec

mergeZones :: (Label a, Ord a) => [Zone a] -> Maybe (Zone a)
mergeZones (concatMap allData -> zs) = foldr go (Just (emptyZone N.root)) zs
  where
    go :: Ord a => Named a b -> Maybe (AZone a b) -> Maybe (AZone a b)
    go r = ((`insert` r) =<<)

noMatching :: Ord a => NamedRecord a -> Zone a -> Bool
noMatching (Named n d) (Zone org z) = maybe True (`go` z) (pathInZone org n)
  where
    go :: Ord a => [a] -> T.Node a RData -> Bool
    go [] (T.Node is _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)
