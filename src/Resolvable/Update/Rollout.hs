{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Rollout
  ( rollout ) where

import Control.Monad (guard)
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import DNS.Diff
import DNS.Name
import DNS.Record
import Resolvable.Dependencies
import DNS.ZoneFile
import NameConstraint
import qualified Util.PartialOrder as PO

type NSDiffs = M.Map (Name String) (ZoneDiff String)

updateDeps :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName
updateDeps diffs ds = PO.filterRights changed (PO.filterLefts changed ds)
  where
    changed :: ResolvableAt ConstrainedName -> Bool
    changed (ResolvableAt (Named s q)) = maybe False (hasChanges q) $ M.lookup s diffs

pickDiffs :: NSDiffs -> ResolvableAt ConstrainedName -> (NSDiffs, [Diff StandardRecord])
pickDiffs m (ResolvableAt (Named n d)) = fromMaybe (m, []) $ do
  z <- M.lookup n m
  let k = toName d
  picked <- get k z
  -- TODO: Below is horrible inefficient. It should be deletion, but it sets the empty list and prunes
  let z' = prune $ set k [] z
  let m' = M.alter (const $ guard (not $ isEmpty z') $> z') n m
  return (m', map (fmap (Named k)) picked)

type Rollout = [[Diff StandardRecord]]

rollout :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout diffs old = rollout' id diffs (PO.transitiveClosure $ PO.invert old) . PO.transitiveClosure
-- Invert @old@ so that we only delete what no one depends on

rollout' :: (Rollout -> Rollout) -> NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout' acc diffs (updateDeps diffs -> old) (updateDeps diffs -> new)
  | M.null diffs = acc []
  | otherwise =
    let canChange r = null (PO.rel r new) && null (PO.rel r old)
        change = filter canChange . map fst
        foldF r (d, l) = (l ++) <$> pickDiffs d r
        facc = foldr foldF (diffs, []) $ change $ PO.toAscList new
        (diffs', changes) = foldr foldF facc $ change $ PO.toAscList old
    in
      if null changes
      -- TODO: Gracefully fail should no rollout be possible
      then error $ "no changes - rolled out so far: " ++ show (acc []) ++ "; remaining: " ++ show (map (fmap allData) $ M.toList diffs')
      else rollout' (acc . (changes :)) diffs' old new
