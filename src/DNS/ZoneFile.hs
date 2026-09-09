{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
module DNS.ZoneFile
  ( singleton
  , emptyZone
  , Path(..)
  , pathInZone
  , AZone(..)
  , isEmpty
  , zshow
  , ZNode
  , ZEdges
  , Zone
  , fromRecordList
  , insert
  , fromList
  , get
  , adjust
  , set
  , prune
  , allData
  , allRRSets
  , allRecords
  , mergeZones
  , noMatching
  , partition
  , allNamed
  , traverseNamed ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Trans.State as St

import DNS.Name as N
import DNS.Record
import qualified DNS.RecordType as T
import qualified DNS.RRSet as RRSet
import qualified Util.Tree as T
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (Foldable(toList))

data AZone a t b = Zone
  { zoneName :: Name a
  , root :: T.Node a (t b) }
  deriving Eq

instance Functor t => Functor (AZone a t) where
  fmap f (Zone n r) = Zone n (fmap (fmap f) r)

zshow :: forall a t b s. Traversable t => ([a] -> b -> St.State s String) -> s -> AZone a t b -> [String]
zshow printf initSt (Zone _ r) = St.evalState (go [] r) initSt
  where
    go  :: [a] -> T.Node a (t b) -> St.State s [String]
    go nameAcc (T.Node d cs) = do
      showD <- mapM (printf nameAcc) (toList d)
      showCs <- concat <$> mapM (\(l, n) -> go (l:nameAcc) n) (M.toAscList cs)
      return $ showD ++ showCs

instance (Eq a, Label a, Traversable t, Show b) => Show (AZone a t b) where
  show = unlines . zshow printf ()
    where
      printf ls d = return $ maybe "@" (show . Name) (NE.nonEmpty ls) ++ " " ++ show d

type ZEdges a = T.Edges a RRSet.RDataSet
type ZNode a = T.Node a RRSet.RDataSet
type Zone a = AZone a RRSet.RTypedSet RData

emptyZone :: Monoid (t b) => Name a -> AZone a t b
emptyZone org = Zone org T.emptyNode

singleton :: Named a (t b) -> AZone a t b
singleton (Named n d) = Zone n (T.leaf d)

isEmpty :: Foldable t => AZone a t b -> Bool
isEmpty (Zone _ r) = T.null r

fromRecordList :: forall a. (Label a, Ord a) => NE.NonEmpty (Record a) -> Zone a
fromRecordList = fromList . NE.map namedRec
  where
    namedRec :: Record a -> Named a (RRSet.RTypedSet RData)
    namedRec (Named n d) = Named n (RRSet.singleton d)

data Path a = Path
  -- New origin of the zone; longest common ancestor between new and old name
  (Name a)
  -- Path of the old data in the zone
  [a]
  -- Path of the new data in the zone
  [a]
  deriving Eq

instance Show (Path String) where
  show (Path n l r) = "Path (" ++ show n ++ ") " ++ show l ++ " " ++ show r

pathInZone :: (Label a, Eq a) => Name a -> Name a -> Path a
pathInZone (Name (NE.reverse -> (orgr NE.:| org))) (Name (NE.reverse -> (nr NE.:| n)))
  | orgr == nr && nr == lempty = pathInZone' N.root org n
  | otherwise = error "at least one name is relative"
  where
    pathInZone' :: Eq a => Name a -> [a] -> [a] -> Path a
    pathInZone' acc [] bs = Path acc [] bs
    pathInZone' acc as [] = Path acc as []
    pathInZone' acc as@(a:asT) bs@(b:bsT)
      | a == b = pathInZone' (a <| acc) asT bsT
      | otherwise = Path acc as bs

mergeZones :: (Label a, Ord a) => Zone a -> Zone a -> Zone a
mergeZones (Zone org1 t1) (Zone org2 t2) = case pathInZone org1 org2 of
  Path org1' p1 p2 -> Zone org1'
    $ T.merge (T.prepend p1 t1) (T.prepend p2 t2)

insert :: (Label a, Ord a, Monoid (t b)) => AZone a t b -> Named a (t b) -> AZone a t b
insert (Zone norg zroot) (Named n d) = case pathInZone norg n of
  Path norg' zp rp -> Zone norg'
    $ T.insert rp d
    $ T.insertNode zp zroot T.emptyNode

fromList :: (Label a, Ord a, Monoid (t b)) => NE.NonEmpty (Named a (t b)) -> AZone a t b
fromList (r0 NE.:| rs) = foldr (flip insert) (singleton r0) rs

get :: (Label a, Ord a) => Name a -> AZone a t b -> Maybe (t b)
get n (Zone org r) = case pathInZone org n of
  Path _ [] p -> T.get p r
  _ -> Nothing

adjust :: (Label a, Ord a) => ([a] -> T.Node a (t b) -> T.Node a (t b)) -> Name a -> AZone a t b -> AZone a t b
adjust f nam z@(Zone org r) = case pathInZone org nam of
  Path _ [] p -> Zone org (T.adjust f p r)
  _ -> z

set :: forall a t b. (Label a, Ord a, Monoid (t b)) => Name a -> t b -> AZone a t b -> AZone a t b
set nam is = adjust setF nam
  where
    setF :: [a] -> T.Node a (t b) -> T.Node a (t b)
    setF [] n = n { T.item = is }
    setF (h:t) n@(T.Node _ cs) = n { T.children = M.insert h (T.leafAt t is) cs }

prune :: AZone a RRSet.RTypedSet b -> AZone a RRSet.RTypedSet b
prune z@(Zone _ r) = z { DNS.ZoneFile.root = T.prune r }

allData :: Foldable t => AZone a t b -> [Named a (t b)]
allData (Zone orig ns) = go orig ns
  where
    go :: Foldable t => Name a -> T.Node a (t b) -> [Named a (t b)]
    go n (T.Node d ch) =
      let base = Named n d
          rec = concatMap (uncurry (\l -> go (l <| n))) (M.toAscList ch)
      in base : rec

allRRSets :: Zone a -> [RRSet.RRSet a]
allRRSets = concatMap (\(Named n typed) -> Named n <$> M.elems (RRSet.rds typed)) . allData

allRecords :: Zone a -> [Record a]
allRecords = concatMap (NE.toList . RRSet.toRecords) . allRRSets

noMatching :: (Label a, Ord a) => Record a -> Zone a -> Bool
noMatching (Named n d) (Zone org z) = case pathInZone org n of
  Path _ [] p -> go p z
  _ -> False
  where
    go :: Ord a => [a] -> T.Node a RRSet.RDataSet -> Bool
    go [] (T.Node is _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)

partition :: (Name a -> T.Type -> NE.NonEmpty b -> Bool) -> AZone a RRSet.RTypedSet b -> (AZone a RRSet.RTypedSet b, AZone a RRSet.RTypedSet b)
partition p (Zone org r) = bimap (Zone org) (Zone org) (T.partition (RRSet.partition . p . (`prependLabels` org)) r)

allNamed :: (Name a -> RRSet.RDataSet -> Bool) -> Zone a -> Bool
allNamed f (Zone org r) = T.allNamed f org r

traverseNamed :: Applicative f => (Name a -> t1 u -> f (t2 v)) -> AZone a t1 u -> f (AZone a t2 v)
traverseNamed f (Zone org r) = Zone org <$> T.traverseNamed f org r
