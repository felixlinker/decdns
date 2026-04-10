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
  , Zone
  , fromRecordList
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
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Trans.State as St

import DNS.Name as N
import DNS.Record
import qualified DNS.RecordType as Ty
import qualified Util.Tree as T
import Data.Bifunctor (Bifunctor(bimap))

data AZone a b = Zone
  { zoneName :: Name a
  , root :: T.Node a [b] }
  deriving Eq

instance Functor (AZone a) where
  fmap f (Zone n r) = Zone n (fmap (fmap f) r)

zshow :: forall a b s. ([a] -> b -> St.State s String) -> s -> AZone a b -> [String]
zshow printf initSt (Zone _ r) = St.evalState (go [] r) initSt
  where
    go  :: [a] -> T.Node a [b] -> St.State s [String]
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

singleton :: Named a b -> AZone a b
singleton (Named n d) = Zone n (T.leaf [d])

isEmpty :: AZone a b -> Bool
isEmpty (Zone _ r) = T.null r

fromRecordList :: forall a. (Label a, Ord a) => [Record a] -> [Zone a]
fromRecordList rList =
  let (soas, other) = L.partition ((== Ty.SOA) . toType . rtype . value) rList
  in  maybe [] (\z -> toZones (foldr insertRecord z other)) (fromList (map namedRec soas))
  where
    toZones :: AZone a (Record a) -> [Zone a]
    toZones (Zone _ rn) = go rn []
      where
        go :: T.Node a [Record a] -> [Zone a] -> [Zone a]
        go (T.Node rs cs) acc =
          let rec = foldr go acc cs
          in  maybe rec (:rec) (fromList rs)

    namedRec :: Record a -> Named a (Record a)
    namedRec r@(Named n _) = Named n r

    insertRecord :: Record a -> AZone a (Record a) -> AZone a (Record a)
    insertRecord r@(Named n _) z@(Zone org rn) = z { root = T.adjust (const (T.mapItems (r:))) p rn }
      where
        p :: [a]
        (Path _ _ p) = pathInZone org n

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
    $ T.insertNode p2 t2
    $ T.insertNode p1 t1 T.emptyNode

insert :: (Label a, Ord a) => AZone a b -> Named a b -> AZone a b
insert (Zone norg zroot) (Named n d) = case pathInZone norg n of
  Path norg' zp rp -> Zone norg'
    $ T.insert rp [d]
    $ T.insertNode zp zroot T.emptyNode

fromList :: (Label a, Ord a) => [Named a b] -> Maybe (AZone a b)
fromList = fmap (uncurry (foldl insert . singleton)) . L.uncons

get :: (Label a, Ord a) => Name a -> AZone a b -> Maybe [b]
get n (Zone org r) = case pathInZone org n of
  Path _ [] p -> T.get p r
  _ -> Nothing

set :: forall a b. (Label a, Ord a) => Name a -> [b] -> AZone a b -> AZone a b
set nam is z@(Zone org r) = case pathInZone org nam of
  Path _ [] p -> Zone org (T.adjust setF p r)
  _ -> z
  where
    setF :: [a] -> T.Node a [b] -> T.Node a [b]
    setF [] n = n { T.item = is }
    setF (h:t) n@(T.Node _ cs) = n { T.children = M.insert h (T.leafAt t is) cs }

prune :: AZone a b -> AZone a b
prune z@(Zone _ r) = z { DNS.ZoneFile.root = T.prune r }

allData :: AZone a b -> [Named a b]
allData (Zone orig ns) = go orig ns
  where
    go :: Name a -> T.Node a [b] -> [Named a b]
    go n (T.Node d ch) =
      let base = map (Named n) d
          rec = concatMap (uncurry (\l -> go (l <| n))) (M.toAscList ch)
      in base ++ rec

noMatching :: (Label a, Ord a) => NamedRecord a -> Zone a -> Bool
noMatching (Named n d) (Zone org z) = case pathInZone org n of
  Path _ [] p -> go p z
  _ -> False
  where
    go :: Ord a => [a] -> T.Node a [RData] -> Bool
    go [] (T.Node is _) = all ((/= rtype d) . rtype) is
    go (h:t) (T.Node _ cs) = maybe True (go t) (M.lookup h cs)

partition :: (Name a -> b -> Bool) -> AZone a b -> (AZone a b, AZone a b)
partition p (Zone org r) = bimap (Zone org) (Zone org) (T.partition (p . (`prependLabels` org)) r)
