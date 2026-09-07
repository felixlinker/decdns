{-# LANGUAGE ViewPatterns #-}
module DNS.Diff
  ( Diff(..)
  , added
  , deleted
  , deither
  , item
  , ZoneDiff
  , diff
  , hasChanges ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name as N
import DNS.Record
import qualified DNS.RecordType as T
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile

import qualified Util.Tree as T
import Data.Bifunctor (Bifunctor(bimap))
import Data.Bifoldable (Bifoldable(bifoldr))
import Data.Bitraversable (Bitraversable(bitraverse))

-- Left is deleted, Right is added
newtype Diff a = Diff
  { diffEither :: Either a a }
  deriving (Eq, Ord)

instance Functor Diff where
  fmap f (Diff e) = Diff $ bimap f f e

instance Foldable Diff where
  foldr f acc (Diff e) = bifoldr f f acc e

instance Traversable Diff where
  traverse f (Diff e) = Diff <$> bitraverse f f e

instance Show a => Show (Diff a) where
  show (Diff (Left a)) = "delete: " ++ show a
  show (Diff (Right a)) = "add: " ++ show a

deleted :: a -> Diff a
deleted = Diff . Left

added :: a  -> Diff a
added = Diff . Right

deither :: (a -> b) -> (a -> b) -> Diff a -> b
deither l r = either l r . diffEither

item :: Diff a -> a
item (Diff (Left a)) = a
item (Diff (Right a)) = a

type ZoneDiff a = AZone a RRSet.RTypedSet (Diff RData)
type RDataSetDiff = RRSet.RTypedSet (Diff RData)

diff :: (Label a, Ord a) => Zone a -> Zone a -> ZoneDiff a
diff (fmap deleted -> Zone oldOrg oldRs) (fmap added -> Zone newOrg newRs) =
  let Path zOrg oldP newP = pathInZone oldOrg newOrg
      oldRs' = T.prepend oldP oldRs
      newRs' = T.prepend newP newRs
  in prune $ Zone zOrg $ T.mapItems (RRSet.modifyL (M.map nubEither)) $ T.merge oldRs' newRs'
  where
    nubEither :: Eq a => NE.NonEmpty (Diff a) -> [Diff a]
    nubEither es = NE.filter (deither ((`all` es) . rightNeq) ((`all` es) . leftNeq)) es

    leftNeq :: Eq a => a -> Diff a -> Bool
    leftNeq x = deither (/= x) true

    rightNeq :: Eq a => a -> Diff a -> Bool
    rightNeq x = deither true (/= x)

    true :: a -> Bool
    true = const True

hasChanges :: (Label a, Ord a) => Name a -> T.Type -> ZoneDiff a -> Bool
hasChanges n ty (Zone org ns) = case pathInZone org n of
  Path _ [] p -> go p ns
  _ -> False
  where
    go :: (Label a, Ord a) => [a] -> T.Node a RDataSetDiff -> Bool
    go [] (T.Node is _) = RRSet.hasType ty is
    go (l : t) node = rec l t node

    rec :: (Label a, Ord a) => a -> [a] -> T.Node a RDataSetDiff -> Bool
    rec l r (T.Node _ cs) = maybe False (go r) (M.lookup l cs)
