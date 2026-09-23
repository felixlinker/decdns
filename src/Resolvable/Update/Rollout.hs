{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Update.Rollout
  ( NSDiffs
  , Diffs
  , RecordDependencies
  , fromResolutionOrder
  , diffServers
  , Rollout
  , showRollOut
  , rollout ) where

import Control.Monad (when, foldM)
import Control.Monad.Trans.Except (Except, throwE)
import qualified Control.Monad.Trans.State as St
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM
import qualified Data.Set as S

import DNS.Diff
import DNS.Name
import qualified DNS.RRSet as RRSet
import Resolvable.Prolog.Core ( Rule(rannotation) )
import Resolvable.Resolvability
import DNS.ZoneFile
import NameConstraint
import qualified Util.Graph as G
import Resolvable.Dependencies (ResolutionOrder)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Util.List (foldrWith)
import Util.Map (alterDefF)
import Util.Fixpoint
import Util.Functor (split)
import DNS.Record (RData)
import Util.Set (checkInsert)

type RRSetID = (Name String, Name String)

type RecordDependencies = G.Graph RRSetID

fromResolutionOrder :: ResolutionOrder -> Maybe RecordDependencies
fromResolutionOrder =
    G.transitiveClosure
  . G.irreflexive
  . G.map (fromAnnot . fromJust . rannotation)
  . G.filter (isJust . rannotation)
  where
    fromAnnot :: RComp -> RRSetID
    fromAnnot (RComp ns rrs) = (ns, lshow <$> name rrs)

type NSDiffs = M.Map (Name String) (ZoneDiff String)
type Diffs = M.Map RRSetID (S.Set (Diff RData))

insertDiff :: RRSetID -> Diff RData -> Diffs -> Diffs
insertDiff k v = M.alter (Just . maybe (S.singleton v) (S.insert v)) k

checkInsertDiff :: RRSetID -> Diff RData -> Diffs -> Changed Diffs
checkInsertDiff k v = M.alterF (fmap Just . checkInsert v . fromMaybe S.empty) k

insertDiffs :: Diffs -> Diffs -> Diffs
insertDiffs ds = foldr (\(k, vs) acc -> foldr (insertDiff k) acc vs) ds . M.toList

checkInsertDiffs :: Diffs -> Diffs -> Changed Diffs
checkInsertDiffs ds = foldM (\acc (k, vs) -> foldM (flip $ checkInsertDiff k) acc vs) ds . M.toList

flattenDiffs :: NSDiffs -> Diffs
flattenDiffs = M.fromList . concatMap (uncurry aux) . M.toList
  where
    aux :: Name String -> ZoneDiff String -> [(RRSetID, S.Set (Diff RData))]
    aux ns = filter (not . S.null . snd) . map (\(Named n d) -> ((ns, n), S.fromList (RRSet.flattenData d))) . allData

showDiffsAtNS :: (Name String, Name String) -> [Diff RData] -> [String]
showDiffsAtNS (ns, n) ds =
    map indent
  $ ("on server " ++ show ns ++ ":")
    : indent (show n) : map (indent . indent . show) ds
  where
    indent :: String -> String
    indent s = if null s then "" else "  " ++ s

showDiffs :: Diffs -> [String]
showDiffs = concatMap (uncurry showDiffsAtNS) . M.toList . M.map S.toList

diffServers :: NameServersConfig -> NameServersConfig -> NSDiffs
diffServers old new = MM.merge
  (MM.mapMissing (\_ -> fmap deleted))
  (MM.mapMissing (\_ -> fmap added))
  (MM.zipWithMatched (const diff))
  (M.fromListWith mergeZones $ map (fmap literalWildcards) old)
  (M.fromListWith mergeZones $ map (fmap literalWildcards) new)

updateDeps :: Diffs -> RecordDependencies -> RecordDependencies
updateDeps diffs = G.filter (`S.member ` M.keysSet diffs)

type Rollout = [Diffs]

showRollOut :: Rollout -> [String]
showRollOut r = do
  (i, diffs) <- zip [(1 :: Int)..] r
  ("step " ++ show i ++ "; roll out:") : showDiffs diffs

type NSMap = M.Map (Name String) (Zone ConstrainedLabel)

removeIdentities :: Diffs -> Diffs
removeIdentities = M.map (foldr consInverseNub S.empty)
  where
    consInverseNub :: Ord a => Diff a -> S.Set (Diff a) -> S.Set (Diff a)
    consInverseNub a acc =
      let withoutInverse = S.alterF (\found -> if found then Just False else Nothing) (invert a) acc
      in fromMaybe (S.insert a acc) withoutInverse

invertAndSplit :: Diffs -> (Diffs, Diffs)
invertAndSplit = split (S.partition isAdded . S.map invert)

rollout :: NSMap -> NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout zones diffs (G.invert -> old) new = do
  updateSeq <- rollout' id (flattenDiffs diffs) old new
  -- Compute initial sequence of updates
  let updateSeq' = filter (not . M.null) $ foldr delayDeletions [] updateSeq
  let updateSeq'' = wildcardSynthesis zones updateSeq'
  return $ map removeIdentities updateSeq''
  where
    delayDeletions :: Diffs -> Rollout -> Rollout
    delayDeletions step acc =
      let (dels, others)  = M.partition (all isDeleted) step
          (acc', here)    = insertDeletions dels acc
      in  others : here : acc'

    insertDeletions :: Diffs -> Rollout -> (Rollout, Diffs)
    insertDeletions ins [] = ([ins], M.empty)
    insertDeletions ins (step:t) =
      let (cannotIns, ins') = M.partitionWithKey (\k _ -> hasDependencies k) ins
          (t', insHere) = insertDeletions ins' t
      in  (insertDiffs step insHere : t', cannotIns)
      where
        hasDependencies :: RRSetID -> Bool
        hasDependencies rid = any (\rid' -> rid `S.member` G.next old rid' || rid `S.member` G.next new rid') (M.keys step)

wildcardSynthesis :: NSMap -> Rollout -> Rollout
wildcardSynthesis orgZ = aux [] orgZ
  where
    aux :: Rollout -> NSMap -> Rollout -> Rollout
    aux accSeq _    []            = accSeq
    aux accSeq cfg  (step:steps)  =
      let (cfg', synthesized) = St.runState (foldM applyStep cfg (M.toList step)) M.empty
          (step', didChange)  = runChanged $ checkInsertDiffs step synthesized
          (addInv, delInv)    = invertAndSplit synthesized
          (accSeq', rest1)    = foldrWith updateRolloutStep addInv accSeq
          (steps', rest2)     = foldrWith updateRolloutStep delInv steps
      in  if    didChange
          then  aux [] orgZ (accSeq' ++ [insertDiffs (insertDiffs step' rest1) rest2] ++ steps')
          else  aux (accSeq ++ [step]) cfg' steps

    applyStep :: NSMap -> ((Name String, Name String), S.Set (Diff RData)) -> St.State Diffs NSMap
    applyStep configAcc ((ns, n), ds) = alterDefF patchSynthesize ns configAcc
      where
        -- TODO: Might require invariant that there is only one RRSetDiff
        -- per name server. Cleaner would be to refactor Diffs so that it is
        -- a map.
        patchSynthesize :: Zone ConstrainedLabel -> St.State Diffs (Zone ConstrainedLabel)
        patchSynthesize z = do
          let (z', wcDiffs) = patch z [Named n ds]
          let wcDiffsWithWC = S.mapMonotonic (mapName (lwildcard <|)) wcDiffs
          St.modify (\st -> foldr (\(Named dn d) -> insertDiff (ns, dn) d) st (wcDiffs <> wcDiffsWithWC))
          return z'

    updateRolloutStep :: Diffs -> Diffs -> (Diffs, Diffs)
    updateRolloutStep step guards =
      let (ins, guards') = M.partitionWithKey (\(ns, n) _ -> any (\(ns', compileName -> n') -> ns == ns' && synthesize n n') (M.keys step)) guards
      in  (insertDiffs step ins, guards')

rollout' :: (Rollout -> Rollout) -> Diffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout' acc diffs (updateDeps diffs -> old) (updateDeps diffs -> new)
  | M.null diffs = return $ acc []
  | otherwise = do
    let (changes, next) = fixpoint rolloutNow (M.empty, diffs)
    when (null changes) (throwE $ unlines $ "no changes; rolled out so far:" : showRollOut (acc []) ++ ["remaining changes:"] ++ showDiffs next)
    rollout' (acc . (changes :)) next old new
  where
    rolloutNow :: (Diffs, Diffs) -> Changed (Diffs, Diffs)
    rolloutNow (apply, wait) = do
      let (newApply, wait') = M.partitionWithKey (\(ns, n) d -> canRollout (summarize d) (ns, n)) wait
      apply' <- checkInsertDiffs apply newApply
      return (apply', wait')
      where
        canRollout :: Change -> (Name String, Name String) -> Bool
        canRollout Unmodified _ = True  -- technically undefined
        canRollout Deleted n    = G.leaf old n
        canRollout _ n          = all (`S.member` M.keysSet apply) (G.next new n)
