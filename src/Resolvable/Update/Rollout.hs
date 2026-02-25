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
updateDeps diffs o@(Deps old) n@(Deps new) =
  let unchangedLeafs = S.filter (not . changed) (leafs n)
      unchangedRoots = S.filter (not . changed) (roots o)
      old' = Deps (foldr M.delete old unchangedRoots)
      new' = Deps (M.map (S.\\ unchangedLeafs) new)
  in if null unchangedLeafs && null unchangedRoots
    then (o, n)
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
  let m' = M.insert n (adjustData (filter (`notElem` picked)) k z) m
  return (m', map (fmap (Named k)) picked)
  where
    -- @pick@ picks the right "extracting" function for a diff based on
    -- whether we want to add or deletes the records associated with the
    -- name
    pick :: Diff a -> Maybe (Diff a)
    pick d' = deither (const (deither Just (const Nothing))) (const (deither (const Nothing) Just)) d d' $> d'

rollout :: NSDiffs -> Dependencies ConstrainedName -> Dependencies ConstrainedName -> [[Diff StandardRecord]]
rollout diffs old new
  | M.null diffs = []
  | otherwise =
    let (Deps old', Deps new') = updateDeps diffs old new
        canAdd r ds = maybe True null (M.lookup r old') && null ds
        canDel r _ = maybe True null (M.lookup r new')

        addNow = filter (uncurry canAdd) (M.toAscList new')
        delNow = filter (uncurry canDel) (M.toAscList old')

        foldF r (d, l) = (l ++) <$> pickDiffs d r
        acc = foldr (foldF . fmap added . fst) (diffs, []) addNow
        (diffs', changes) = foldr (foldF . fmap deleted . fst) acc delNow
    in changes : rollout diffs' (Deps old') (Deps new')
