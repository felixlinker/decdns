{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Rollout
  ( NSDiffs
  , RecordDependencies
  , fromResolutionOrder
  , diffServers
  , Rollout
  , showRollOut
  , rollout ) where

import Control.Monad (when)
import Control.Monad.Trans.Except (Except, throwE)
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

import DNS.Diff
import DNS.Name
import Resolvable.Prolog.Core ( Rule(rannotation) )
import Resolvable.Resolvability
import DNS.ZoneFile
import NameConstraint
import qualified Util.Graph as G
import Resolvable.Dependencies (ResolutionOrder)
import Data.Maybe (isJust, fromJust)
import Util.Bool ((==>))

type RRSetID = (Name String, ConstrainedName)

type RecordDependencies = G.Graph RRSetID

fromResolutionOrder :: ResolutionOrder -> Maybe RecordDependencies
fromResolutionOrder =
    G.transitiveClosure
  . G.irreflexive
  . G.map (fromAnnot . fromJust . rannotation)
  . G.filter (isJust . rannotation)
  where
    fromAnnot :: RComp -> RRSetID
    fromAnnot (RComp ns rrs) = (ns, name rrs)

canRollout :: RecordDependencies -> Name String -> Name String -> Bool
canRollout deps ns n = all matchingWildcardIsLeaf deps
  where
    matchingWildcardIsLeaf :: RRSetID -> Bool
    matchingWildcardIsLeaf node@(ns', n') = (ns == ns' && overlap (compileNoContext n) n') ==> G.leaf deps node

type NSDiffs = M.Map (Name String) (ZoneDiff String)

showZoneDiff :: Name String -> ZoneDiff String -> [String]
showZoneDiff ns d =
    map indent
  $ ("on server " ++ show ns ++ ":")
    : map indent (lines (show d))
  where
    indent :: String -> String
    indent s = if null s then "" else "  " ++ s

showNSDiffs :: NSDiffs -> [String]
showNSDiffs = M.foldrWithKey (\ns d -> (showZoneDiff ns d ++)) []

diffServers :: NameServersConfig -> NameServersConfig -> NSDiffs
diffServers old new = MM.merge
  (MM.mapMissing (\_ -> fmap deleted))
  (MM.mapMissing (\_ -> fmap added))
  (MM.zipWithMatched (const diff))
  (M.fromListWith mergeZones $ map (fmap literalWildcards) old)
  (M.fromListWith mergeZones $ map (fmap literalWildcards) new)

updateDeps :: NSDiffs -> RecordDependencies -> RecordDependencies
updateDeps diffs = G.filter changed
  where
    changed :: RRSetID -> Bool
    changed (ns, qn) = maybe False (hasChanges $ fmap lshow qn) (M.lookup ns diffs)

type Rollout = [NSDiffs]

showRollOut :: Rollout -> [String]
showRollOut r = do
  (i, diffs) <- zip [(1 :: Int)..] r
  ("step " ++ show i ++ "; roll out:") : showNSDiffs diffs

rollout :: NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout diffs (G.invert -> old) new = do
  rollout' id diffs old new

rollout' :: (Rollout -> Rollout) -> NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout' acc (normalize -> diffs) (updateDeps diffs -> old) (updateDeps diffs -> new)
  | M.null diffs = return $ acc []
  | otherwise = do
    let (changes, next) = partitionDiffs diffs
    when (null changes) (throwE $ unlines $ "no changes; rolled out so far:" : showRollOut (acc []) ++ ["remaining changes:"] ++ showNSDiffs next)
    rollout' (acc . (changes :)) next old new
  where
    partitionDiffs :: NSDiffs -> (NSDiffs, NSDiffs)
    partitionDiffs ds =
      let diffs' = M.mapWithKey partitionDiff ds
      in (normalize (M.map fst diffs'), normalize (M.map snd diffs'))

    partitionDiff :: Name String -> ZoneDiff String -> (ZoneDiff String, ZoneDiff String)
    partitionDiff ns = partition (\n _ _ -> canRollout new ns n && canRollout old ns n)

dropEmpty :: NSDiffs -> NSDiffs
dropEmpty = M.filter (not . isEmpty)

normalize :: NSDiffs -> NSDiffs
normalize = dropEmpty . M.map prune
