module PrologCompiler (main) where

import DNS.ZoneFile (allRecords, mergeZones)
import DNS.Dig (followUpQueries)
import DNS.Wellformedness ( zoneWellformed, responseWellformed )
import NameConstraint (toStandardRecord)
import Resolvable.Prolog.Core
import Resolvable.Resolvability
import Resolvable.Update.Rollout (diffServers, showRollOut, rollout, fromResolutionOrder)
import Resolvable.Dependencies (computeResolvable, resolutionOrder)
import Util.Fresh (runFresh)
import Util.Monad (failLeftS, handleErrorS, handleMaybe)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Map as M
import qualified Data.Set as S
import Resolvable.Update.Dependencies (computeRegressions)
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Parsing (parseNameM, parseTypeM)
import Parsing.Config (parseZoneFiles)
import Query (queryStep)
import Visualizer (renderVisualization)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      putStr (showKnowledgeBase $ S.toList $ runFresh $ compileConfig rootServers cfg)
    "resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      let compiled = runFresh $ plResolvable =<< compileConfig rootServers cfg
      let res = computeResolvable True compiled
      putStrLn $ showKnowledgeBase $ S.toList res
    "diff" -> do
      (old, new) <- sepBy "--" args
      oldRecs <- parseZones old
      newRecs <- parseZones new
      let nsDiff = diffServers oldRecs newRecs
      mapM_ (\(ns, d) -> print ns >> print d) (M.toList nsDiff)
    "regressions" -> do
      (sroots, args') <- sepBy "--" args
      roots <- parseNames sroots
      (old, new) <- sepBy "--" args'
      oldRecs <- parseZones old
      newRecs <- parseZones new
      let originalZones = M.fromListWith mergeZones oldRecs
      let oldRecsC = compileConfig roots oldRecs
      let newRecsC = compileConfig roots newRecs
      regs <- failLeftS $ runFresh $ runExceptT $ computeRegressions (plResolvable =<< oldRecsC) (plResolvable =<< newRecsC)
      mapM_ print regs
      let nsDiff = diffServers oldRecs newRecs
      oldDeps <- handleMaybe "cyclic old resolution order" $ runFresh $ fromResolutionOrder . resolutionOrder <$> oldRecsC
      newDeps <- handleMaybe "cyclic new resolution order" $ runFresh $ fromResolutionOrder . resolutionOrder <$> newRecsC
      rolloutSeq <- handleErrorS (rollout originalZones nsDiff oldDeps newDeps)
      unless (null rolloutSeq) $
        putStrLn $ unlines $ map ("% " ++) $ "You can roll out updates as follow." :  showRollOut rolloutSeq
    "visualize" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      let compiled = runFresh $ compileConfig rootServers cfg
      putStrLn $ renderVisualization rootServers zones cfg compiled
    "parse-only" -> do
      parsed <- concatMap (map toStandardRecord . allRecords . snd) <$> parseZones args
      mapM_ print parsed
    "query" -> do
      (roots, [qnameString, qtypeString]) <- sepBy "--" args
      rootServers <- parseNames roots
      qname <- parseNameM qnameString
      qtype <- parseTypeM qtypeString
      compiled <- compileConfig rootServers <$> parseResponse
      let kb = queryStep qname qtype compiled
      let answers = filter isFact $ S.toList kb
      if null answers
        then mapM_ print $ followUpQueries kb
        else putStrLn $ showKnowledgeBase answers
    _ -> fail "unknown command"
  where
    parseZones :: [FilePath] -> IO NameServersConfig
    parseZones = parseZoneFiles zoneWellformed

    parseResponse :: IO NameServersConfig
    parseResponse = parseStdin responseWellformed

    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
