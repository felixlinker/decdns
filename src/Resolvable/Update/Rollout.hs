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
type Diffs = S.Set (Name String, Named String RDataSetDiff)

flattenDiffs :: NSDiffs -> Diffs
flattenDiffs = S.fromList . concatMap (\(ns, z) -> map (ns,) $ filter (not . RRSet.null . value) (allData z)) . M.toList

showDiffsAtNS :: Name String -> [Named String RDataSetDiff] -> [String]
showDiffsAtNS ns ds =
    map indent
  $ ("on server " ++ show ns ++ ":")
    : concatMap (\(Named n d) -> indent (show n) : map (indent . indent . show) (RRSet.flattenData d)) ds
  where
    indent :: String -> String
    indent s = if null s then "" else "  " ++ s

showDiffs :: Diffs -> [String]
showDiffs = concatMap (uncurry showDiffsAtNS) . M.toList . foldr (\(ns, d) -> M.alter (Just . maybe [d] (d:)) ns) M.empty

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

showRollOut :: Rollout -> [String]
showRollOut r = do
  (i, diffs) <- zip [(1 :: Int)..] r
  ("step " ++ show i ++ "; roll out:") : showDiffs diffs

rollout :: NSDiffs -> RecordDependencies -> RecordDependencies -> Except String Rollout
rollout diffs old new = do
  rollout' id (flattenDiffs diffs) (G.invert old) new

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
        canRollout (ns, n) = G.leaf old (ns, n) && all (`S.member` toIDs wait) (G.next new (ns, n))

    checkInsert :: Ord a => a -> S.Set a -> Changed (S.Set a)
    checkInsert = S.alterF (\found -> St.modify (not found ||) >> return True)
