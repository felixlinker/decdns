{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DNS.ZoneFile
  ( emptyZone
  , Path(..)
  , pathInZone
  , AZone(..)
  , isEmpty
  , zshow
  , Zone
  , insert
  , fromList
  , get
  , set
  , prune
  , allData
  , mergeZones
  , noMatching
  , partition ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Trans.State as St

import DNS.Name as N
import DNS.Record
import qualified Util.Tree as T
import Data.Bifunctor (Bifunctor(bimap))

data AZone a b = Zone
  { zoneName :: Name a
  , root :: T.Node a b }

instance Functor (AZone a) where
  fmap f (Zone n r) = Zone n (fmap f r)

zshow :: forall a b s. ([a] -> b -> St.State s String) -> s -> AZone a b -> [String]
zshow printf initSt (Zone _ r) = St.evalState (go [] r) initSt
  where
    go  :: [a] -> T.Node a b -> St.State s [String]
    go nameAcc (T.Node d cs) = do
      showD <- mapM (printf nameAcc) d
      showCs <- concat <$> mapM (\(l, n) -> go (l:nameAcc) n) (M.toAscList cs)
      return $ showD ++ showCs

instance (Show (Name a), Show b) => Show (AZone a b) where
  show = unlines . zshow printf ()
    where
      printf ls d = return $ maybe "@" (show . Name) (NE.nonEmpty ls) ++ " " ++ show d

type Zone a = AZone a RData

emptyZone :: Name a -> AZone a b
emptyZone org = Zone org T.emptyNode

isEmpty :: AZone a b -> Bool
isEmpty (Zone _ r) = T.null r

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

fromList :: (Label a, Ord a) => [Named a b] -> AZone a b
fromList = foldl insert (emptyZone N.root)

get :: (Label a, Ord a) => Name a -> AZone a b -> Maybe [b]
get n (Zone org r) = case pathInZone org n of
  Path _ [] p -> T.get p r
  _ -> Nothing

set :: (Label a, Ord a) => Name a -> [b] -> AZone a b -> AZone a b
set nam is z@(Zone org r) = case pathInZone org nam of
  Path _ [] p -> Zone org (T.alter (const (T.leaf is)) (Just . (`T.leafAt` is)) p r)
  _ -> z

prune :: AZone a b -> AZone a b
prune z@(Zone _ r) = z { DNS.ZoneFile.root = T.prune r }

allData :: AZone a b -> [Named a b]
allData (Zone orig ns) = go orig ns
  where
    go :: Name a -> T.Node a b -> [Named a b]
    go n (T.Node d ch) =
      let base = map (Named n) d
          rec = concatMap (uncurry (\l -> go (l <| n))) (M.toAscList ch)
      in base ++ rec

noMatching :: (Label a, Ord a) => NamedRecord a -> Zone a -> Bool
noMatching (Named n d) (Zone org z) = case pathInZone org n of
  Path _ [] p -> go p z
  _ -> False
  where
    go :: Ord a => [a] -> T.Node a RData -> Bool
    go [] (T.Node is _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)

partition :: (Name a -> b -> Bool) -> AZone a b -> (AZone a b, AZone a b)
partition p (Zone org r) = bimap (Zone org) (Zone org) (T.partition (p . (`prependLabels` org)) r)
