module PrologCompiler (main) where

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Update.Resolvability
import Resolvable.Dependencies
import Util.Fresh (runFresh)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import Resolvable.Update.Dependencies (computeRegressions)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- mapM parseZoneFileZones zones
      putStr (showKnowledgeBase $ runFresh $ plResolvable rootServers cfg)
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
      compiled <- compileConfig rootServers <$> mapM parseZoneFileZones zones
      let us = runFresh $ computeResolvable rootServers compiled
      putStr $ unlines $ map show us
    "regressions" -> do
      (roots, args') <- sepBy "--" args
      rootServers <- parseNames roots
      (old, new) <- sepBy "--" args'
      oldRs <- plResolvable rootServers <$> mapM parseZoneFileZones old
      newRs <- plResolvable rootServers <$> mapM parseZoneFileZones new
      putStr $ unlines $ map show $ computeRegressions rootServers oldRs newRs
    "dependencies" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      compiled <- runFresh . compileConfig rootServers <$> mapM parseZoneFileZones zones
      print $ fromRules compiled
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
