module DNS.Diff
  ( Diff
  , diff ) where

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
