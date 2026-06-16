{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Update.Rollout
  ( NSDiffs
  , diffServers
  , Rollout
  , rollout ) where

import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM
import qualified Data.Set as S

import DNS.Diff
import DNS.Name
import Resolvable.Dependencies
import Resolvable.Resolvability
import DNS.ZoneFile
import qualified DNS.RecordType as T
import NameConstraint
import qualified Util.PartialOrder as PO
import Util.Func
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromMaybe)
import DNS.RRSet (RTypedSet(rds))

canRollout :: RecordDependencies -> Name String -> ConstrainedName -> Bool
canRollout deps ns n = S.null $ PO.smaller deps $ ResolvableAt (ns, n)

type NSDiffs = M.Map (Name String) (ZoneDiff ConstrainedLabel)

diffServers :: NameServersConfig -> NameServersConfig -> NSDiffs
diffServers old new = MM.merge
  (MM.mapMissing (\_ -> fmap deleted))
  (MM.mapMissing (\_ -> fmap added))
  (MM.zipWithMatched (const diff))
  (M.fromList $ map (fmap toSingleZone) old)
  (M.fromList $ map (fmap toSingleZone) new)
  where
    def :: Zone ConstrainedLabel
    def = emptyZone DNS.Name.root

    toSingleZone :: [Zone ConstrainedLabel] -> Zone ConstrainedLabel
    toSingleZone = foldr (mergeZones . fromMaybe def . fromList . allData) def

updateDeps :: NSDiffs -> RecordDependencies -> RecordDependencies
updateDeps diffs = PO.filter changed
  where
    changed :: ResolvableAt -> Bool
    changed (ResolvableAt (ns, qn)) = maybe False (hasChanges qn) (M.lookup ns diffs)

type Rollout = [NSDiffs]

rollout :: NSDiffs -> RecordDependencies -> RecordDependencies -> Maybe Rollout
rollout diffs old new = do
  old' <- PO.transitiveClosure $ PO.invert old
  new' <- PO.transitiveClosure new
  return $ rollout' id diffs old' new'
-- Invert @old@ so that we only delete what no one depends on

rollout' :: (Rollout -> Rollout) -> NSDiffs -> RecordDependencies -> RecordDependencies -> Rollout
rollout' acc (M.map prune -> diffs) (updateDeps diffs -> old) (updateDeps diffs -> new)
  | M.null diffs = acc []
  | otherwise = let (M.map prune -> changes, next) = partitionDiffs diffs
                in  if null changes
                    -- TODO: Gracefully fail should no rollout be possible
                    then error "no changes"
                    else rollout' (acc . (changes :)) next old new
  where
    partitionDiffs :: NSDiffs -> (NSDiffs, NSDiffs)
    partitionDiffs ds =
      let diffs' = M.mapWithKey partitionDiff ds
      in (dropEmpty (M.map fst diffs'), dropEmpty (M.map snd diffs'))

    dropEmpty :: NSDiffs -> NSDiffs
    dropEmpty = M.filter (not . isEmpty)

    partitionDiff :: Name String -> ZoneDiff ConstrainedLabel -> (ZoneDiff ConstrainedLabel, ZoneDiff ConstrainedLabel)
    partitionDiff ns = partition (\n _ -> canRollout new ns n && canRollout old ns n)
