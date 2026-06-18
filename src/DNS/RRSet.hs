module DNS.RRSet
  ( RTypedSet(..)
  , empty
  , apply
  , modify
  , modifyL
  , partition
  , RDataSet
  , insert
  , singleton
  , RRSet
  , StdRRSet
  , canonicalRRSet
  , toRecords ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor.Identity (Identity(runIdentity))

newtype RTypedSet a = RDS
  { rds :: M.Map T.Type (NE.NonEmpty a) }
  deriving (Eq, Ord)

instance Show a => Show (RTypedSet a) where
  show (RDS m) = show (M.elems m)

empty :: RTypedSet a
empty = RDS M.empty

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

apply :: (M.Map T.Type (NE.NonEmpty a) -> b) -> RTypedSet a -> b
apply f (RDS m) = f m

modify :: (M.Map T.Type (NE.NonEmpty a) -> M.Map T.Type (NE.NonEmpty b)) -> RTypedSet a -> RTypedSet b
modify f = RDS . apply f

toNE :: M.Map T.Type [a] -> M.Map T.Type (NE.NonEmpty a)
toNE = runIdentity . M.traverseMaybeWithKey (const $ return . NE.nonEmpty) . M.filter (not . null)

modifyL :: (M.Map T.Type (NE.NonEmpty a) -> M.Map T.Type [b]) -> RTypedSet a -> RTypedSet b
modifyL f rd = RDS $ toNE $ apply f rd

partition :: (a -> Bool) -> RTypedSet a -> (RTypedSet a, RTypedSet a)
partition p rd =
  let ps = apply (M.map $ NE.partition p) rd
  in bimap (RDS . toNE) (RDS . toNE) (fmap fst ps, fmap snd ps)

type RDataSet = RTypedSet RData
type RRSet a = Named a (NE.NonEmpty RData)
type StdRRSet = RRSet String

canonicalRRSet :: StdRRSet -> StdRRSet
canonicalRRSet (Named n set) = Named (canonicalName n) (fmap canonicalRData set)

insert :: RData -> RDataSet -> RDataSet
insert rd = RDS . M.alter (Just . maybe (NE.singleton rd) (rd NE.<|)) (toType $ rtype rd) . rds

singleton :: RData -> RDataSet
singleton rd = RDS $ M.singleton (toType $ rtype rd) (NE.singleton rd)

toRecords :: RRSet a -> NE.NonEmpty (Record a)
toRecords (Named n sets) = Named n <$> sets
