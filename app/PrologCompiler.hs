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
import Resolvable.Update.Trivial (partitionTrivial)
import Control.Monad (unless)

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
      (sroots, args') <- sepBy "--" args
      roots <- parseNames sroots
      (old, new) <- sepBy "--" args'
      oldRecs <- mapM parseZoneFileZones old
      newRecs <- mapM parseZoneFileZones new
      let (trivial, nonTrivial) = runFresh $ partitionTrivial (concatMap snd oldRecs) (concatMap snd newRecs) $ computeRegressions roots (plResolvable roots oldRecs) (plResolvable roots newRecs)
      putStr $ unlines $ map show nonTrivial
      unless (null trivial) $ do
        putStrLn "\nThe following regressions may be inteded as all records that directly answer the query have been deleted."
        putStr $ unlines $ map show trivial
    "dependencies" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      compiled <- runFresh . compileConfig rootServers <$> mapM parseZoneFileZones zones
      print $ fromRules compiled
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
