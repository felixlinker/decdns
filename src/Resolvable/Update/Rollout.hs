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
import Util.Map (groupBy, alterDefF)

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

rollout :: NSMap -> NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout zones diffs (G.invert -> old) new = do
  updateSeq <- rollout' id (flattenDiffs diffs) old new
  -- Compute initial sequence of updates
  let updateSeq' = filter (not . S.null) $ foldr delayDeletions [] updateSeq
  -- Apply updates to zone and check whether we must account for temporary
  -- changes in wildcard semantics. ($ []) runs the accumulator.
  let (_, ($ []) -> wildcardGuards) = foldl' applyStep (zones, id) (map groupDiffs updateSeq')
  let updateSeq'' = foldl' insertGuards [] (zip updateSeq' wildcardGuards)
  return $ map removeIdentities updateSeq''
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
      in  (step <> insHere : t', cannotIns)
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
          St.modify (\st -> foldr (S.insert . (ns,)) st wcDiffs)
          return z'

    insertGuards :: Rollout -> (Diffs, Diffs) -> Rollout
    insertGuards acc (step, guards) =
      -- Insert inverses into the first possible rollout step. This might fail,
      -- in that case, add the inverses right here.
      let (acc', remainingInverses) = foldl' insertGuardsStep ([], invertDiffs guards) acc
      in  acc' ++ [step <> guards <> remainingInverses]

    insertGuardsStep :: (Rollout, Diffs) -> Diffs -> (Rollout, Diffs)
    insertGuardsStep (accR, guards) rolloutStep =
      let (rolloutStep', guards') = updateRolloutStep rolloutStep guards
      in  (accR ++ [rolloutStep'], guards')

    updateRolloutStep :: Diffs -> Diffs -> (Diffs, Diffs)
    updateRolloutStep step guards =
      let (ins, guards') = S.partition (\(fmap name -> g) -> any ((== g) . fmap name) step) guards
      in  (step <> ins, guards')

rollout' :: (Rollout -> Rollout) -> Diffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout' acc diffs (updateDeps diffs -> old) (updateDeps diffs -> new)
  | S.null diffs = return $ acc []
  | otherwise = do
    let (changes, next) = rolloutNow diffs
    when (null changes) (throwE $ unlines $ "no changes; rolled out so far:" : showRollOut (acc []) ++ ["remaining changes:"] ++ showDiffs next)
    rollout' (acc . (changes :)) next old new
  where
    rolloutNow :: Diffs -> (Diffs, Diffs)
    rolloutNow = do S.partition (canRollout . fmap name)
      where
        canRollout :: RRSetID -> Bool
        canRollout (ns, n) = G.leaf old (ns, n) && G.leaf new (ns, n)
