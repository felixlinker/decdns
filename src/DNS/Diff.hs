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

import DNS.Name as N
import DNS.Record
import DNS.ZoneFile

import qualified Util.Tree as T
import Data.Bifunctor (Bifunctor(bimap))
import NameConstraint

-- Left is deleted, Right is added
newtype Diff a = Diff
  { diffEither :: Either a a }
  deriving (Eq, Ord)

instance Functor Diff where
  fmap f (Diff e) = Diff $ bimap f f e

instance Applicative Diff where
  pure a = Diff $ Left a
  (Diff (Left f)) <*> d = fmap f d
  (Diff (Right f)) <*> d = fmap f d

deleted :: a -> Diff a
deleted = Diff . Left

added :: a  -> Diff a
added = Diff . Right

deither :: (a -> b) -> (a -> b) -> Diff a -> b
deither l r = either l r . diffEither

item :: Diff a -> a
item (Diff (Left a)) = a
item (Diff (Right a)) = a

type ZoneDiff a = AZone a (Diff RData)

diff :: (Label a, Ord a) => Zone a -> Zone a -> ZoneDiff a
diff (Zone oldOrg (fmap deleted -> oldZ)) (Zone newOrg (fmap added -> newZ))
  | oldOrg == newOrg = Zone N.root $ T.mapItems nubEither $ T.merge oldZ newZ
  -- TODO: For now. I expect diff to be called after mergeZones anyway, which
  -- creates a zone for the root name anyways.
  | otherwise = error "diff must be called on two zones of the same origin"
  where
    nubEither :: Eq a => [Diff a] -> [Diff a]
    nubEither es = filter (deither ((`all` es) . rightNeq) ((`all` es) . leftNeq)) es

    leftNeq :: Eq a => a -> Diff a -> Bool
    leftNeq x = deither (/= x) true

    rightNeq :: Eq a => a -> Diff a -> Bool
    rightNeq x = deither true (/= x)

    true :: a -> Bool
    true = const True

hasChanges :: (Label a, Ord a) => Name (LabelConstraint a) -> ZoneDiff a -> Bool
hasChanges n (Zone org ns) = case pathInZone (exactly org) n of
  Path _ [] p -> go p ns
  _ -> False
  where
    go :: (Label a, Ord a) => [LabelConstraint a] -> T.Node a (Diff RData) -> Bool
    go [] (T.Node is _) = not $ null is
    go [Wildcard ls] node@(T.Node _ cs) =
          -- Either there is a wildcard label with changes
          rec lwildcard [] node
          -- Or there is a label matched by the wildcard with changes
      ||  any (\(l, T.items -> is) -> not (null is) && notElem l ls) (M.toList cs)
    go (Wildcard _ : _) _ = undefined  -- Cannot have wildcard within a name
    go (Exactly l : t) node = rec l t node

    rec :: (Label a, Ord a) => a -> [LabelConstraint a] -> T.Node a (Diff RData) -> Bool
    rec l r (T.Node _ cs) = maybe False (go r) (M.lookup l cs)
