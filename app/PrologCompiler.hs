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
    "prolog" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- mapM parseZoneFileZones zones
      putStr (showKnowledgeBase (plResolvable rootServers cfg))
    "delta" -> do
      (roots, args') <- sepBy "--" args
      rootServers <- parseNames roots
      (old, new) <- sepBy "--" args'
      oldCfg <- mapM parseZoneFileZones old
      newCfg <- mapM parseZoneFileZones new
      putStr (showKnowledgeBase (plRegressions rootServers oldCfg newCfg))
    "deps" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      parsed <- mapM parseZoneFileZones zones
      let compiled = runFresh $ compileConfig rootServers parsed
      print (fromRules compiled)
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
      oldRs <- compileConfig rootServers <$> mapM parseZoneFileZones old
      newRs <- compileConfig rootServers <$> mapM parseZoneFileZones new
      putStr $ unlines $ map show $ computeRegressions rootServers oldRs newRs
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
