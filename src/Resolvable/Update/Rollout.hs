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
import Util.Fixpoint
import Util.Map (groupBy, alterDefF)
import Data.Bifunctor (Bifunctor(bimap))

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

rollout :: NSMap -> NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout zones diffs old new = do
  updateSeq <- rollout' id (flattenDiffs diffs) (G.invert old) new
  let (_, ($ []) -> synthesized) = foldl' applyStep (zones, id) (map groupDiffs updateSeq)
  let resolvabilityGuards = map fst synthesized
  return $ foldGuards resolvabilityGuards [] $ zipWith S.union updateSeq (map snd synthesized)
  -- return updateSeq
  where
    applyStep :: (NSMap, [(Diffs, Diffs)] -> [(Diffs, Diffs)]) -> M.Map (Name String) [RRSetDiff] -> (NSMap, [(Diffs, Diffs)] -> [(Diffs, Diffs)])
    applyStep (config, acc) ds =
      let (config', synthesized) = St.runState (foldM applyServer config (M.toList ds)) (S.empty, S.empty)
      in  (config', acc . (synthesized:))

    -- Apply a diff for a name server. Returns the updated name server map, and
    -- stores synthesized responses for occluding wildcards and their inverses
    -- in the state. The left tuple element must be applied before the right
    -- tuple element.
    applyServer :: NSMap -> (Name String, [RRSetDiff]) -> St.State (Diffs, Diffs) NSMap
    applyServer config (ns, ds) = alterDefF patchSynthesize ns config
      where
        biInsertDiff :: RRSetDiff -> (Diffs, Diffs) -> (Diffs, Diffs)
        biInsertDiff wcDiff = bimap (S.insert (ns, fmap invert <$> wcDiff)) (S.insert  (ns, wcDiff))

        patchSynthesize :: Zone ConstrainedLabel -> St.State (Diffs, Diffs) (Zone ConstrainedLabel)
        patchSynthesize z = do
          let (z', wcDiffs) = patch z ds
          St.modify (\st -> foldr biInsertDiff st wcDiffs)
          return z'

    -- Below code takes wildcard guards and inserts them into the last step that
    -- updates the same name.
    foldGuards :: WildcardGuards -> Rollout -> Rollout -> Rollout
    foldGuards [] acc [] = acc
    foldGuards _ _ [] = error "invariant violated: rollout shorter than guards"
    foldGuards [] _ _ = error "invariant violated: guards shorter than rollout"
    foldGuards (guards:restGuards) stepsPrefix (step:restSteps) =
      foldGuards restGuards (insertGuards stepsPrefix ++ [step]) restSteps
      where
        insertGuards :: [Diffs] -> [Diffs]
        insertGuards = fst . foldr insertGuardsStep ([], guards)

    insertGuardsStep :: Diffs -> (Rollout, Diffs) -> (Rollout, Diffs)
    insertGuardsStep rolloutStep (accR, guards) =
      let (rolloutStep', guards') = updateRolloutStep rolloutStep guards
      in  (rolloutStep':accR, guards')

    updateRolloutStep :: Diffs -> Diffs -> (Diffs, Diffs)
    updateRolloutStep step guards =
      let (ins, guards') = S.partition (\(fmap name -> g) -> any ((== g) . fmap name) step) guards
      in  (step <> ins, guards')

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
      let (newApply, wait') = S.partition (canRollout . fmap name) wait
      apply' <- foldM (flip checkInsert) apply newApply
      return (apply', wait')
      where
        canRollout :: RRSetID -> Bool
        canRollout (ns, n) = G.leaf old (ns, n) && all (`S.member` toIDs apply) (G.next new (ns, n))

    checkInsert :: Ord a => a -> S.Set a -> Changed (S.Set a)
    checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)
