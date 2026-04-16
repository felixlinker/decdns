{-# LANGUAGE ViewPatterns #-}
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
import NameConstraint
import qualified Util.PartialOrder as PO
import Util.Func
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromMaybe)

type NSDiffs = M.Map (Name String) (ZoneDiff String)

diffServers :: NameServersConfig -> NameServersConfig -> NSDiffs
diffServers old new = MM.merge
  (MM.mapMissing (\_ -> fmap deleted))
  (MM.mapMissing (\_ -> fmap added))
  (MM.zipWithMatched (const diff))
  (M.fromList $ map (fmap toSingleZone) old)
  (M.fromList $ map (fmap toSingleZone) new)
  where
    def :: Zone String
    def = emptyZone DNS.Name.root

    toSingleZone :: [Zone ConstrainedLabel] -> Zone String
    toSingleZone = foldr (mergeZones . fromMaybe def . fromList . map (mapLabels show) . allData) def

updateDeps :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName
updateDeps diffs ds = PO.filterLefts changed (PO.filterRights changed ds)
  where
    changed :: ResolvableAt ConstrainedName -> Bool
    changed (ResolvableAt (Named s q)) = maybe False (hasChanges q) $ M.lookup s diffs

type Rollout = [NSDiffs]

rollout :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout diffs old = rollout' id diffs (PO.transitiveClosure $ PO.invert old) . PO.transitiveClosure
-- Invert @old@ so that we only delete what no one depends on

rollout' :: (Rollout -> Rollout) -> NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout' acc diffs (updateDeps diffs -> old) (updateDeps diffs -> new)
  | M.null diffs = acc []
  | otherwise = let (changes, next) = partitionDiffs diffs
                in  if null changes
                    -- TODO: Gracefully fail should no rollout be possible
                    then error $ "no changes - rolled out so far: " ++ show (acc []) ++ "; remaining: " ++ show (map (fmap allData) $ M.toList next)
                    else rollout' (acc . (changes :)) next old new
  where
    canChange :: ResolvableAt ConstrainedName -> Bool
    canChange r = S.null (PO.rel r new) && S.null (PO.rel r old)

    partitionDiffs :: NSDiffs -> (NSDiffs, NSDiffs)
    partitionDiffs ds =
      let diffs' = M.mapWithKey partitionDiff ds
      in (dropEmpty (M.map fst diffs'), dropEmpty (M.map snd diffs'))

    dropEmpty :: NSDiffs -> NSDiffs
    dropEmpty = M.filter (not . isEmpty)

    partitionDiff :: Name String -> ZoneDiff String -> (ZoneDiff String, ZoneDiff String)
    partitionDiff ns = bimap prune prune . partition (canChange <.> (\n _ -> ResolvableAt $ Named ns $ exactly n))
