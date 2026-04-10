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

type ZoneDiff a = AZone a (Diff RData)

diff :: (Label a, Ord a) => Zone a -> Zone a -> ZoneDiff a
diff (fmap deleted -> Zone oldOrg oldRs) (fmap added -> Zone newOrg newRs) =
  let Path zOrg oldP newP = pathInZone oldOrg newOrg
      oldRs' = T.prepend oldP oldRs
      newRs' = T.prepend newP newRs
  in prune $ Zone zOrg $ T.mapItems nubEither $ T.merge oldRs' newRs'
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
    go :: (Label a, Ord a) => [LabelConstraint a] -> T.Node a [Diff RData] -> Bool
    go [] (T.Node is _) = not $ null is
    go [Wildcard ls] node@(T.Node _ cs) =
          -- Either there is a wildcard label with changes
          rec lwildcard [] node
          -- Or there is a label matched by the wildcard with changes
      ||  any (\(l, T.item -> is) -> not (null is) && notElem l ls) (M.toList cs)
    go (Wildcard _ : _) _ = undefined  -- Cannot have wildcard within a name
    go (Exactly l : t) node = rec l t node

    rec :: (Label a, Ord a) => a -> [LabelConstraint a] -> T.Node a [Diff RData] -> Bool
    rec l r (T.Node _ cs) = maybe False (go r) (M.lookup l cs)
