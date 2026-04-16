module PrologCompiler (main) where

import DNS.ZoneFile (allRecords)
import DNS.Diff
import NameConstraint (toStandardRecord)
import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Update.Resolvability
import Resolvable.Update.Rollout (diffServers, Rollout, rollout)
import Resolvable.Dependencies ( computeResolvable, fromRules )
import Util.Fresh (runFresh)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Map as M
import Resolvable.Update.Dependencies (computeRegressions)
import Resolvable.Update.Trivial (partitionTrivial)
import Control.Monad (unless)
import DNS.Name (Name)

indent :: String -> String
indent s = if null s then "" else "  " ++ s

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- mapM parseZoneFileZones zones
      putStr (showKnowledgeBase $ runFresh $ plResolvable =<< compileConfig rootServers cfg)
    "pl-regressions" -> do
      (roots, args') <- sepBy "--" args
      rootServers <- parseNames roots
      (old, new) <- sepBy "--" args'
      oldCfg <- mapM parseZoneFileZones old
      newCfg <- mapM parseZoneFileZones new
      putStr (showKnowledgeBase (plRegressions rootServers oldCfg newCfg))
    "resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- mapM parseZoneFileZones zones
      let compiled = plResolvable =<< compileConfig rootServers cfg
      let us = runFresh $ computeResolvable rootServers compiled
      mapM_ print us
    "diff" -> do
      (old, new) <- sepBy "--" args
      oldRecs <- mapM parseZoneFileZones old
      newRecs <- mapM parseZoneFileZones new
      let nsDiff = diffServers oldRecs newRecs
      mapM_ (\(ns, d) -> print ns >> print d) (M.toList nsDiff)
    "regressions" -> do
      (sroots, args') <- sepBy "--" args
      roots <- parseNames sroots
      (old, new) <- sepBy "--" args'
      oldRecs <- mapM parseZoneFileZones old
      newRecs <- mapM parseZoneFileZones new
      let oldRecsC = compileConfig roots oldRecs
      let newRecsC = compileConfig roots newRecs
      let (trivial, nonTrivial) = runFresh $ partitionTrivial (concatMap (concatMap allRecords . snd) oldRecs) (concatMap (concatMap allRecords . snd) newRecs) $ computeRegressions roots (plResolvable =<< oldRecsC) (plResolvable =<< newRecsC)
      mapM_ print nonTrivial
      unless (null trivial) $ do
        putStrLn "\n% The following regressions may be inteded as all records that directly answer the query have been deleted."
        mapM_ print trivial
      let nsDiff = diffServers oldRecs newRecs
      let oldDeps = runFresh $ fromRules <$> oldRecsC
      let newDeps = runFresh $ fromRules <$> newRecsC
      let rolloutSeq = rollout nsDiff oldDeps newDeps
      unless (null rolloutSeq) $ do
        putStrLn "\n% You can roll out updates as follow."
        mapM_ putStrLn (showRollOut rolloutSeq)
    "dependencies" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      compiled <- runFresh . compileConfig rootServers <$> mapM parseZoneFileZones zones
      print $ fromRules compiled
    "parse-only" -> do
      parsed <- concatMap (map toStandardRecord . concatMap allRecords . snd) <$> mapM parseZoneFileZones args
      mapM_ print parsed
    _ -> fail "unknown command"
  where

    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons

    showRollOut :: Rollout -> [String]
    showRollOut r = map ("% " ++) $ do
      (i, ds) <- zip [(1 :: Int)..] r
      ("step " ++ show i) : M.foldrWithKey (\ns d -> (showNSDiffs ns d ++)) [] ds

    showNSDiffs :: Name String -> ZoneDiff String -> [String]
    showNSDiffs ns d =
        map indent
      $ ("on server " ++ show ns ++ " roll out:")
        : map indent (lines (show d))
