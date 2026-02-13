{-# LANGUAGE ViewPatterns #-}
module DNS.Diff
  ( Diff(..)
  , ZoneDiff
  , diff
  , someDeleted ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name as N
import DNS.Record
import DNS.ZoneFile

import qualified Util.Tree as T
import Data.Functor.Identity (Identity(runIdentity))
import Data.Bifunctor (Bifunctor(bimap))

-- Left is deleted, Right is added
newtype Diff a = Diff
  { diffEither :: Either a a }

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

type ZoneDiff = AZone Diff

diff :: (Label a, Ord a) => Zone a -> Zone a -> ZoneDiff a
diff (Zone oldOrg (fmap (deleted . runIdentity) -> oldZ)) (Zone newOrg (fmap (added . runIdentity) -> newZ))
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

someDeleted :: (Label a, Ord a) => NamedRecord a -> ZoneDiff a -> Bool
someDeleted (Record (Name ls) d) (Zone _ ns)
  | r /= lempty = error "record name is relative"
  | otherwise = go p ns
  where
    (r NE.:| p) = NE.reverse ls

    go :: Ord a => [a] -> T.Node a (Diff RData) -> Bool
    go [] (T.Node is _) = any (deither ((== rtype d) . rtype) (const False)) is
    go (h:t) (T.Node _ cs) = maybe False (go t) (M.lookup h cs)
