{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Rollout
  ( Rollout
  , rollout ) where

import qualified Data.Map as M
import qualified Data.Set as S

import DNS.Diff
import DNS.Name
import Resolvable.Dependencies
import DNS.ZoneFile
import NameConstraint
import qualified Util.PartialOrder as PO
import Util.Func
import Data.Bifunctor (Bifunctor(bimap))

type NSDiffs = M.Map (Name String) (ZoneDiff String)

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
