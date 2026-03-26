module Resolvable.Update.Rollout
  ( rollout ) where

import Control.Monad (guard)
import Data.Functor (($>))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import DNS.Diff
import DNS.Name
import DNS.Record
import Resolvable.Dependencies
import DNS.ZoneFile
import NameConstraint
import qualified Util.PartialOrder as PO

type NSDiffs = M.Map (Name String) (ZoneDiff String)

updateDeps :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> (Dependencies ConstrainedName, Dependencies ConstrainedName)
updateDeps diffs old new =
  let unchangedNew = S.filter (not . changed) (PO.maxima new)
      unchangedOld = S.filter (not . changed) (PO.minima old)
      old' = foldr PO.remove old unchangedOld
      new' = foldr PO.remove new unchangedNew
  in if null unchangedNew && null unchangedOld
    then (old, new)
    else updateDeps diffs old' new'
  where
    changed :: ResolvableAt ConstrainedName -> Bool
    changed (ResolvableAt (Named s q)) = maybe False (hasChanges q) $ M.lookup s diffs

pickDiffs :: NSDiffs -> ResolvableAt (Diff ConstrainedName) -> (NSDiffs, [Diff StandardRecord])
pickDiffs m (ResolvableAt (Named n d)) = fromMaybe (m, []) $ do
  z <- M.lookup n m
  let k = toName (item d)
  (picked, is) <- L.partition pick <$> get k z
  let z' = prune $ set k is z
  let m' = M.alter (const $ guard (not $ isEmpty z') $> z') n m
  return (m', map (fmap (Named k)) picked)
  where
    -- @pick@ picks the right "extracting" function for a diff based on
    -- whether we want to add or deletes the records associated with the
    -- name
    pick :: Diff a -> Bool
    pick (Diff (Left _)) = deither (const True) (const False) d
    pick (Diff (Right _)) = deither (const False) (const True) d

type Rollout = [[Diff StandardRecord]]

rollout :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout = rollout' id

rollout' :: (Rollout -> Rollout) -> NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout' acc diffs old new
  | M.null diffs = acc []
  | otherwise =
    let (old', new') = updateDeps diffs old new
        canAdd r ds = null (PO.rel r old') && null ds
        canDel r _ = maybe True null (PO.rel r new')

        addNow = filter (uncurry canChange) (PO.toAscList new')
        delNow = filter (uncurry canChange) (PO.toAscList old')

        foldF r (d, l) = (l ++) <$> pickDiffs d r
        facc = foldr (foldF . fmap added . fst) (diffs, []) addNow
        (diffs', changes) = foldr (foldF . fmap deleted . fst) facc delNow
    in
      if null changes
      then error $ "no changes - rolled out so far: " ++ show (acc [])
      else rollout' ((changes :) . acc) diffs' old' new'
