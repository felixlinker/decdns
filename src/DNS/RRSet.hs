module DNS.RRSet
  ( RTypedSet(..)
  , empty
  , apply
  , modify
  , prune
  , partition
  , RDataSet
  , insert
  , singleton
  , RRSet
  , StdRRSet ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import Data.Bifunctor (Bifunctor(bimap))

newtype RTypedSet a = RDS
  { rds :: M.Map T.Type [a] }
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

apply :: (M.Map T.Type [a] -> b) -> RTypedSet a -> b
apply f (RDS m) = f m

modify :: (M.Map T.Type [a] -> M.Map T.Type [b]) -> RTypedSet a -> RTypedSet b
modify f = RDS . apply f

prune :: RTypedSet a -> RTypedSet a
prune = modify (M.filter (not . null))

partition :: (a -> Bool) -> RTypedSet a -> (RTypedSet a, RTypedSet a)
partition p rd =
  let ps = apply (M.map $ L.partition p) rd
  in bimap (prune . RDS) (prune . RDS) (fmap fst ps, fmap snd ps)

type RDataSet = RTypedSet RData
type RRSet a = Named a RDataSet
type StdRRSet = Named String RDataSet

insert :: RData -> RDataSet -> RDataSet
insert rd = RDS . M.alter (Just . maybe [rd] (rd:)) (toType $ rtype rd) . rds

singleton :: RData -> RDataSet
singleton rd = RDS $ M.singleton (toType $ rtype rd) [rd]
