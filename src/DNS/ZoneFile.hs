{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile
  ( emptyZone
  , AZone(..)
  , Zone
  , insert
  , allRecords
  , mergeZones
  , noMatching ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity

import DNS.Name as N
import DNS.Record
import qualified Util.Tree as T

data AZone f a = Zone
  { zoneName :: Name a
  , root :: T.Node a (f RData) }

type Zone = AZone Identity

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

insert :: (Applicative f, Ord a) => AZone f a -> NamedRecord a -> Maybe (AZone f a)
insert (Zone norg zroot) (Record n d) = do
  p <- pathInZone norg n
  return $ Zone norg (T.insert p (pure d) zroot)

allRecords :: Functor f => AZone f a -> [f (NamedRecord a)]
allRecords (Zone orig ns) = go orig ns
  where
    go :: Functor f => Name a -> T.Node a (f RData) -> [f (NamedRecord a)]
    go n (T.Node rs ch) =
      let base = map (fmap (Record n)) rs
          rec = concat $ M.mapWithKey (\l -> go (l <| n)) ch
      in base ++ rec

mergeZones :: (Label a, Ord a) => [Zone a] -> Maybe (Zone a)
mergeZones (map runIdentity . concatMap allRecords -> zs) = foldr go (Just (emptyZone N.root)) zs
  where
    go :: (Applicative f, Ord a) => NamedRecord a -> Maybe (AZone f a) -> Maybe (AZone f a)
    go r = ((`insert` r) =<<)

noMatching :: Ord a => NamedRecord a -> Zone a -> Bool
noMatching (Record n d) (Zone org z) = maybe True (`go` z) (pathInZone org n)
  where
    go :: Ord a => [a] -> T.Node a (Identity RData) -> Bool
    go [] (T.Node (map runIdentity -> is) _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)
