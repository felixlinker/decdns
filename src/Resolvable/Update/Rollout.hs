module Resolvable.Update.Rollout
  ( rollout ) where

import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Set as S

import DNS.Diff
import DNS.Name
import DNS.Record
import Resolvable.Dependencies
import DNS.ZoneFile
import NameConstraint
import Data.Maybe (mapMaybe, fromMaybe)

type NSDiffs = M.Map (Name String) (ZoneDiff String)

updateDeps :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> (Dependencies ConstrainedName, Dependencies ConstrainedName)
updateDeps diffs old new =
  let unchangedLeafs = S.filter (not . changed) (leafs new)
      unchangedRoots = S.filter (not . changed) (roots old)
      old' = foldr ddelete old unchangedRoots
      new' = foldr ddelete new unchangedLeafs
  in if null unchangedLeafs && null unchangedRoots
    then (old, new)
    else updateDeps diffs old' new'
  where
    changed :: ResolvableAt ConstrainedName -> Bool
    changed (ResolvableAt (Named s q)) = maybe False (hasChanges q) $ M.lookup s diffs

pickDiffs :: NSDiffs -> ResolvableAt (Diff ConstrainedName) -> (NSDiffs, [Diff StandardRecord])
pickDiffs m (ResolvableAt (Named n d)) = fromMaybe (m, []) $ do
  z <- M.lookup n m
  let k = toName (item d)
  is <- get k z
  let picked = mapMaybe pick is
  let m' = M.insert n (prune $ adjustData (filter (`notElem` picked)) k z) m
  return (m', map (fmap (Named k)) picked)
  where
    -- @pick@ picks the right "extracting" function for a diff based on
    -- whether we want to add or deletes the records associated with the
    -- name
    pick :: Diff a -> Maybe (Diff a)
    pick d' = deither (const (deither Just (const Nothing))) (const (deither (const Nothing) Just)) d d' $> d'

type Rollout = [[Diff StandardRecord]]

rollout :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout = rollout' id

rollout' :: (Rollout -> Rollout) -> NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> Rollout
rollout' acc diffs old new
  | M.null diffs = acc []
  | otherwise =
    let (Deps old', Deps new') = updateDeps diffs old new
        canAdd r ds = maybe True null (M.lookup r old') && null ds
        canDel r _ = maybe True null (M.lookup r new')

        addNow = filter (uncurry canAdd) (M.toAscList new')
        delNow = filter (uncurry canDel) (M.toAscList old')

        foldF r (d, l) = (l ++) <$> pickDiffs d r
        facc = foldr (foldF . fmap added . fst) (diffs, []) addNow
        (diffs', changes) = foldr (foldF . fmap deleted . fst) facc delNow
    in
      if null changes
      then error $ "no changes - rolled out so far: " ++ show (acc [])
      else rollout'
        ((changes :) . acc)
        (M.filter (not . isEmpty) diffs')
        (Deps old')
        (Deps new')
