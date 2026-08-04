{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Rollout
  ( NSDiffs
  , diffServers
  , Rollout
  , rollout ) where

import Control.Monad (when)
import Control.Monad.Trans.Except (Except, throwE)
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

import DNS.Diff
import DNS.Name
import Resolvable.Dependencies
import Resolvable.Resolvability
import DNS.ZoneFile
import NameConstraint
import qualified Util.PartialOrder as PO
import Util.Monad (exceptMaybe)

canRollout :: RecordDependencies -> Name String -> ConstrainedName -> Bool
canRollout deps ns n = PO.minimal deps $ ResolvableAt (ns, n)

type NSDiffs = M.Map (Name String) (ZoneDiff ConstrainedLabel)

diffServers :: NameServersConfig -> NameServersConfig -> NSDiffs
diffServers old new = MM.merge
  (MM.mapMissing (\_ -> fmap deleted))
  (MM.mapMissing (\_ -> fmap added))
  (MM.zipWithMatched (const diff))
  (M.fromList old)
  (M.fromList new)

updateDeps :: NSDiffs -> RecordDependencies -> RecordDependencies
updateDeps diffs = PO.filter changed
  where
    changed :: ResolvableAt -> Bool
    changed (ResolvableAt (ns, qn)) = maybe False (hasChanges qn) (M.lookup ns diffs)

type Rollout = [NSDiffs]

rollout :: NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout diffs old new = do
  old' <- exceptMaybe "old dependencies cyclic" $ PO.transitiveClosure $ PO.invert $ PO.irreflexive old
  new' <- exceptMaybe "new dependencies cyclic" $ PO.transitiveClosure $ PO.irreflexive new
  rollout' id diffs old' new'

rollout' :: (Rollout -> Rollout) -> NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout' acc (normalize -> diffs) (updateDeps diffs -> old) (updateDeps diffs -> new)
  | M.null diffs = return $ acc []
  | otherwise = do
    let (changes, next) = partitionDiffs diffs
    when (null changes) (throwE "no changes")
    rollout' (acc . (changes :)) next old new
  where
    partitionDiffs :: NSDiffs -> (NSDiffs, NSDiffs)
    partitionDiffs ds =
      let diffs' = M.mapWithKey partitionDiff ds
      in (normalize (M.map fst diffs'), normalize (M.map snd diffs'))

    partitionDiff :: Name String -> ZoneDiff ConstrainedLabel -> (ZoneDiff ConstrainedLabel, ZoneDiff ConstrainedLabel)
    partitionDiff ns = partition (\n _ -> canRollout new ns n && canRollout old ns n)

dropEmpty :: NSDiffs -> NSDiffs
dropEmpty = M.filter (not . isEmpty)

normalize :: NSDiffs -> NSDiffs
normalize = dropEmpty . M.map prune
