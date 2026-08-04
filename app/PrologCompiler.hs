module PrologCompiler (main) where

import DNS.ZoneFile (allRecords)
import DNS.Diff
import DNS.Dig (followUpQueries)
import DNS.Wellformedness
import NameConstraint (toStandardRecord, ConstrainedLabel)
import Resolvable.Prolog.Core
import Resolvable.Resolvability
import Resolvable.Update.Resolvability
import Resolvable.Update.Rollout (diffServers, Rollout, rollout)
import Resolvable.Dependencies (computeResolvable, fromRules)
import Util.Fresh (runFresh)
import Util.Monad (failLeft, handleError)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Map as M
import qualified Data.Set as S
import Resolvable.Update.Dependencies (computeRegressions)
import Control.Monad (unless)
import DNS.Name (Name)
import Control.Monad.Trans.Except (runExceptT)
import Parsing (parseNameM, parseTypeM)
import Parsing.Config (parseZoneFiles)
import Query (queryStep)
import Visualizer (renderVisualization)

indent :: String -> String
indent s = if null s then "" else "  " ++ s

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      putStr (showKnowledgeBase $ S.toList $ runFresh $ plResolvable =<< compileConfig rootServers cfg)
    "pl-regressions" -> do
      (roots, args') <- sepBy "--" args
      rootServers <- parseNames roots
      (old, new) <- sepBy "--" args'
      oldCfg <- parseZones old
      newCfg <- parseZones new
      putStr (showKnowledgeBase $ S.toList $ plRegressions rootServers oldCfg newCfg)
    "resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      let compiled = runFresh $ plResolvable =<< compileConfig rootServers cfg
      res <- handleError (computeResolvable True compiled)
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
      let oldRecsC = compileConfig roots oldRecs
      let newRecsC = compileConfig roots newRecs
      regs <- failLeft $ runFresh $ runExceptT $ computeRegressions (plResolvable =<< oldRecsC) (plResolvable =<< newRecsC)
      mapM_ print regs
      let nsDiff = diffServers oldRecs newRecs
      let oldDeps = runFresh $ fromRules <$> oldRecsC
      let newDeps = runFresh $ fromRules <$> newRecsC
      rolloutSeq <- handleError (rollout nsDiff oldDeps newDeps)
      unless (null rolloutSeq) $
        putStrLn $ unlines $ "\n% You can roll out updates as follow." : showRollOut rolloutSeq
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
      kb <- handleError $ queryStep qname qtype compiled
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

    showRollOut :: Rollout -> [String]
    showRollOut r = map ("% " ++) $ do
      (i, diffs) <- zip [(1 :: Int)..] r
      ("step " ++ show i) : M.foldrWithKey (\ns d -> (showNSDiffs ns d ++)) [] diffs

    showNSDiffs :: Name String -> ZoneDiff ConstrainedLabel -> [String]
    showNSDiffs ns d =
        map indent
      $ ("on server " ++ show ns ++ " roll out:")
        : map indent (lines (show d))
