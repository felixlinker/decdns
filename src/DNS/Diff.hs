module DNS.Diff
  ( Diff
  , diff
  , someDeleted ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.Record
import DNS.ZoneFile

import qualified Util.Tree as T

-- Left is deleted, Right is added
type Diff a = T.Node a (Either RData RData)

diff :: Ord a => Zone a -> Zone a -> Diff a
diff (Zone oldOrg oldZ) (Zone newOrg newZ)
  | oldOrg == newOrg = T.mapItems nubEither $ T.merge (fmap Left oldZ) (fmap Right newZ)
  -- TODO: For now. I expect diff to be called after mergeZones anyway, which
  -- creates a zone for the root name anyways.
  | otherwise = error "diff must be called on two zones of the same origin"
  where
    nubEither :: Eq a => [Either a a] -> [Either a a]
    nubEither es = filter (either ((`all` es) . rightNeq) ((`all` es) . leftNeq)) es

    leftNeq :: Eq a => a -> Either a a -> Bool
    leftNeq x = either (/= x) true

    rightNeq :: Eq a => a -> Either a a -> Bool
    rightNeq x = either true (/= x)

    true :: a -> Bool
    true = const True

someDeleted :: (Ord a, Monoid a) => Record (Name a) -> Diff a -> Bool
someDeleted (Record (Name ls) d)
  | r /= mempty = error "record name is relative"
  | otherwise = go p
  where
    (r NE.:| p) = NE.reverse ls

    go :: Ord a => [a] -> Diff a -> Bool
    go [] (T.Node is _) = any (either ((== rtype d) . rtype) (const False)) is
    go (h:t) (T.Node _ cs) = maybe False (go t) (M.lookup h cs)
