{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DNS.RRSet
  ( RTypedSet(..)
  , empty
  , DNS.RRSet.null
  , modify
  , modifyL
  , partition
  , DNS.RRSet.map
  , DNS.RRSet.filter
  , RDataSet
  , insert
  , singleton
  , fromList
  , flattenData
  , RRSet
  , StdRRSet
  , canonicalRRSet
  , toRecords
  , rrsetType
  , getAliased
  , getIP
  , hasType
  , getType
  , nsNames
  , dropType ) where

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(runIdentity))
import Data.IP (IP(IPv4, IPv6))
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import Util.List (snub)

newtype RTypedSet a = RDS
  { rds :: M.Map T.Type (NE.NonEmpty a) }
  deriving (Eq, Ord)

instance Show a => Show (RTypedSet a) where
  show (RDS m) = show (M.elems m)

empty :: RTypedSet a
empty = RDS M.empty

null :: RTypedSet a -> Bool
null = M.null . rds

instance Functor RTypedSet where
  fmap f (RDS m) = RDS $ fmap (fmap f) m

instance Foldable RTypedSet where
  foldr f b (RDS m) = foldr (flip (foldr f)) b m

instance Traversable RTypedSet where
  traverse f (RDS m) = RDS <$> traverse (traverse f) m

instance Ord a => Semigroup (RTypedSet a) where
  (RDS s1) <> (RDS s2) = RDS $ MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched $ const (<>)) s1 s2

instance Ord a => Monoid (RTypedSet a) where
  mempty = RDS M.empty

modify :: (M.Map T.Type (NE.NonEmpty a) -> M.Map T.Type (NE.NonEmpty b)) -> RTypedSet a -> RTypedSet b
modify f = RDS . f . rds

toNE :: M.Map T.Type [a] -> M.Map T.Type (NE.NonEmpty a)
toNE = runIdentity . M.traverseMaybeWithKey (const $ return . NE.nonEmpty) . M.filter (not . Prelude.null)

modifyL :: (M.Map T.Type (NE.NonEmpty a) -> M.Map T.Type [b]) -> RTypedSet a -> RTypedSet b
modifyL f = RDS . toNE . f . rds

partition :: (T.Type -> NE.NonEmpty a -> Bool) -> RTypedSet a -> (RTypedSet a, RTypedSet a)
partition p (rds -> d) = bimap RDS RDS (M.partitionWithKey p d)

filter :: (a -> Bool) -> RTypedSet a -> RTypedSet a
filter p = RDS . M.mapMaybe (NE.nonEmpty . NE.filter p) . rds

map :: (a -> b) -> RTypedSet a -> RTypedSet b
map f = RDS . M.map (NE.map f) . rds

type RDataSet = RTypedSet RData
type RRSet a = Named a (NE.NonEmpty RData)
type StdRRSet = RRSet String

canonicalRRSet :: StdRRSet -> StdRRSet
canonicalRRSet (Named n set) = Named (canonicalName n) (fmap canonicalRData set)

insert :: RData -> RDataSet -> RDataSet
insert rd = RDS . M.alter (Just . maybe (NE.singleton rd) (rd NE.<|)) (toType $ rtype rd) . rds

singleton :: RData -> RDataSet
singleton rd = RDS $ M.singleton (toType $ rtype rd) (NE.singleton rd)

fromList :: [RData] -> RDataSet
fromList = foldr insert (RDS M.empty)

flattenData :: RTypedSet a -> [a]
flattenData = concatMap NE.toList . M.elems . rds

toRecords :: RRSet a -> NE.NonEmpty (Record a)
toRecords (Named n sets) = Named n <$> sets

rrsetType :: RRSet a -> T.Type
rrsetType = toType . rtype . NE.head . value

getAliased :: RRSet a -> Maybe (Name String)
getAliased (Named _ ((rtype -> CNAME n) NE.:| [])) = Just n
getAliased (Named _ ((rtype -> DNAME n) NE.:| [])) = Just n
getAliased _ = Nothing

getIP :: forall a. RRSet a -> [(Name a, IP)]
getIP (Named n rs) = concatMap (asIP . rtype) $ NE.toList rs
  where
    asIP :: Type -> [(Name a, IP)]
    asIP (A ip) = [(n, IPv4 ip)]
    asIP (AAAA ip) = [(n, IPv6 ip)]
    asIP _ = []

hasType :: T.Type -> RTypedSet a -> Bool
hasType t = M.member t . rds

getType :: T.Type -> RDataSet -> [RData]
getType t = maybe [] NE.toList . M.lookup t . rds

nsNames :: Foldable f => f RData -> [Name String]
nsNames rs = snub [canonicalName ns | RData _ _ (NS ns) <- toList rs]

dropType :: T.Type -> RTypedSet a -> RTypedSet a
dropType t = modify (M.delete t)
