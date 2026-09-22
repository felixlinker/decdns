{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Update.Rollout
  ( NSDiffs
  , Diffs
  , flattenDiffs
  , RecordDependencies
  , fromResolutionOrder
  , diffServers
  , Rollout
  , showRollOut
  , rollout ) where

import Control.Monad (when, foldM)
import Control.Monad.Trans.Except (Except, throwE)
import qualified Control.Monad.Trans.State as St
import Data.Foldable (Foldable(foldl'))
import qualified Data.List.NonEmpty as NE
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
import Data.Maybe (isJust, fromJust)
import Util.List (foldrWith)
import Util.Map (groupBy, alterDefF)
import Util.Set (mapMonotonicF)
import Util.Fixpoint
import DNS.Record (RData)

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
type Diffs = S.Set (Name String, RRSetDiff)

flattenDiffs :: NSDiffs -> Diffs
flattenDiffs = S.fromList . concatMap (\(ns, z) -> map (ns,) $ filter (not . RRSet.null . value) (allData z)) . M.toList

insertDiff :: (Name String, RRSetDiff) -> Diffs -> Diffs
insertDiff c@(ns, Named n d) = onChange id (S.insert c) . mapMonotonicF checkInsert
  where
    checkInsert :: (Name String, RRSetDiff) -> Changed (Name String, RRSetDiff)
    checkInsert c'@(ns', Named n' d')
      | ns == ns' && n == n' = changed (ns', Named n' (d' <> d))
      | otherwise = unchanged c'

showDiffsAtNS :: Name String -> [RRSetDiff] -> [String]
showDiffsAtNS ns ds =
    map indent
  $ ("on server " ++ show ns ++ ":")
    : concatMap (\(Named n d) -> indent (show n) : map (indent . indent . show) (RRSet.flattenData d)) ds
  where
    indent :: String -> String
    indent s = if null s then "" else "  " ++ s

showDiffs :: Diffs -> [String]
showDiffs = concatMap (uncurry showDiffsAtNS) . M.toList . groupDiffs

groupDiffs :: Diffs -> M.Map (Name String) [RRSetDiff]
groupDiffs = M.map (map snd) . groupBy fst

diffServers :: NameServersConfig -> NameServersConfig -> NSDiffs
diffServers old new = MM.merge
  (MM.mapMissing (\_ -> fmap deleted))
  (MM.mapMissing (\_ -> fmap added))
  (MM.zipWithMatched (const diff))
  (M.fromListWith mergeZones $ map (fmap literalWildcards) old)
  (M.fromListWith mergeZones $ map (fmap literalWildcards) new)

toIDs :: Diffs -> S.Set RRSetID
toIDs = S.mapMonotonic (fmap name)

updateDeps :: Diffs -> RecordDependencies -> RecordDependencies
updateDeps diffs = G.filter (`S.member ` toIDs diffs)

type Rollout = [Diffs]
type WildcardGuards = Rollout

showRollOut :: Rollout -> [String]
showRollOut r = do
  (i, diffs) <- zip [(1 :: Int)..] r
  ("step " ++ show i ++ "; roll out:") : showDiffs diffs

type NSMap = M.Map (Name String) (Zone ConstrainedLabel)

-- fmaps correspond to: Tuple then Named then RRSet.RTypedSet
invertDiffs :: Diffs -> Diffs
invertDiffs = S.map $ fmap $ fmap $ fmap invert

removeIdentities :: Diffs -> Diffs
removeIdentities = S.map (fmap (fmap (RRSet.modify (M.mapMaybe removeIdentities'))))
  where
    removeIdentities' :: Eq a => NE.NonEmpty (Diff a) -> Maybe (NE.NonEmpty (Diff a))
    removeIdentities' = NE.nonEmpty . foldr consInverseNub []

    consInverseNub :: Eq a => Diff a -> [Diff a] -> [Diff a]
    consInverseNub a acc =
      let inverse = invert a
      in if inverse `elem` acc then filter (/= inverse) acc else a:acc

invertAndSplit :: WildcardGuards -> (WildcardGuards, WildcardGuards)
invertAndSplit gs =
  let inv = map invertDiffs gs
  in (map (pick isAdded) inv, map (pick isDeleted) inv)
  where
    pick :: (Diff RData -> Bool) -> Diffs -> Diffs
    pick p = S.filter (not . RRSet.null . value . snd) . S.map (fmap (fmap (RRSet.filter p)))

rollout :: NSMap -> NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout zones diffs (G.invert -> old) new = do
  updateSeq <- rollout' id (flattenDiffs diffs) old new
  -- Compute initial sequence of updates
  let updateSeq' = filter (not . S.null) $ foldr delayDeletions [] updateSeq
  -- Apply updates to zone and check whether we must account for temporary
  -- changes in wildcard semantics. ($ []) runs the accumulator.
  let (_, ($ []) -> wildcardGuards) = foldl' applyStep (zones, id) (map groupDiffs updateSeq')
  let (addInvs, delInvs) = invertAndSplit wildcardGuards
  let updateSeq'' = foldr insertInverse [] (zip updateSeq' delInvs)
  let updateSeq''' = reverse $ foldr insertInverse [] (zip (reverse updateSeq'') (reverse addInvs))
  -- Insert guards at the index where the change in semantics occured; do this
  -- after previous step so that the place where the name changed remains
  -- unchanged.
  let updateSeq'''' = zipWith (foldr insertDiff) wildcardGuards updateSeq'''
  return $ map removeIdentities updateSeq''''
  where
    -- Applies a rollout step to a config. Also synthesizes wildcard guards and their inv
    applyStep :: (NSMap, WildcardGuards -> WildcardGuards) -> M.Map (Name String) [RRSetDiff] -> (NSMap, WildcardGuards -> WildcardGuards)
    applyStep (config, acc) step =
      let (config', synthesized) = St.runState (foldM applyServer config (M.toList step)) S.empty
      -- Use (acc . (_:)) accumulator so that the resulting list's order
      -- corresponds to the input list's order.
      in  (config', acc . (synthesized:))

    delayDeletions :: Diffs -> Rollout -> Rollout
    delayDeletions step acc =
      let (dels, others)  = S.partition (all isDeleted . value . snd) step
          (acc', here)    = insertDeletions dels acc
      in  others : here : acc'

    insertDeletions :: Diffs -> Rollout -> (Rollout, Diffs)
    insertDeletions ins [] = ([ins], S.empty)
    insertDeletions ins (step:t) =
      let (cannotIns, ins') = S.partition (hasDependencies . fmap name) ins
          (t', insHere) = insertDeletions ins' t
      in  (foldr insertDiff step insHere : t', cannotIns)
      where
        hasDependencies :: RRSetID -> Bool
        hasDependencies rid = any (\(fmap name -> rid') -> rid `S.member` G.next old rid' || rid `S.member` G.next new rid') step

    -- Apply a diff for a name server. Returns the updated name server map, and
    -- stores synthesized responses for occluding wildcards and their inverses
    -- in the state. The left tuple element must be applied before the right
    -- tuple element.
    applyServer :: NSMap -> (Name String, [RRSetDiff]) -> St.State Diffs NSMap
    applyServer config (ns, ds) = alterDefF patchSynthesize ns config
      where
        patchSynthesize :: Zone ConstrainedLabel -> St.State Diffs (Zone ConstrainedLabel)
        patchSynthesize z = do
          let (z', wcDiffs) = patch z ds
          let wcDiffsWithWC = S.mapMonotonic (mapName (lwildcard <|)) wcDiffs
          St.modify (\st -> foldr (insertDiff . (ns,)) st (wcDiffs <> wcDiffsWithWC))
          return z'

    insertInverse :: (Diffs, Diffs) -> Rollout -> Rollout
    insertInverse (s, ds) []  = [foldr insertDiff s ds]
    insertInverse (s, ds) acc
      | S.null ds = s : acc
      | otherwise =
        let (acc', ds') = foldrWith updateRolloutStep ds acc
        in  foldr insertDiff s ds' : acc'

    updateRolloutStep :: Diffs -> Diffs -> (Diffs, Diffs)
    updateRolloutStep step guards =
      let (ins, guards') = S.partition (\(ns, name -> n) -> any (\(ns', compileName . name -> n') -> ns == ns' && synthesize n n') step) guards
      in  (foldr insertDiff step ins, guards')

rollout' :: (Rollout -> Rollout) -> Diffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout' acc diffs (updateDeps diffs -> old) (updateDeps diffs -> new)
  | S.null diffs = return $ acc []
  | otherwise = do
    let (changes, next) = fixpoint rolloutNow (S.empty, diffs)
    when (null changes) (throwE $ unlines $ "no changes; rolled out so far:" : showRollOut (acc []) ++ ["remaining changes:"] ++ showDiffs next)
    rollout' (acc . (changes :)) next old new
  where
    rolloutNow :: (Diffs, Diffs) -> Changed (Diffs, Diffs)
    rolloutNow (apply, wait) = do
      let (newApply, wait') = S.partition (\(ns, d) -> canRollout (summarize (value d)) (ns, name d)) wait
      apply' <- foldM (flip checkInsert) apply newApply
      return (apply', wait')
      where
        canRollout :: Change -> (Name String, Name String) -> Bool
        canRollout Unmodified _ = True  -- technically undefined
        canRollout Deleted n    = G.leaf old n
        canRollout _ n          = all (`S.member` toIDs apply) (G.next new n)

    checkInsert :: Ord a => a -> S.Set a -> Changed (S.Set a)
    checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)
