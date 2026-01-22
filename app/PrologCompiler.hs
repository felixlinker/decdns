module PrologCompiler (main) where

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Update.Resolvability
import Resolvable.Dependencies
import Util.Fresh (runFresh)

import System.Environment (getArgs)
import Util
import Data.List (uncons)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "prolog" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- mapM parseZoneFileZones zones
      putStrLn (showKnowledgeBase (plResolvable rootServers cfg))
    "delta" -> do
      (roots, args') <- sepBy "--" args
      rootServers <- parseNames roots
      (old, new) <- sepBy "--" args'
      oldCfg <- mapM parseZoneFileZones old
      newCfg <- mapM parseZoneFileZones new
      putStrLn (showKnowledgeBase (plRegressions rootServers oldCfg newCfg))
    "deps" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      parsed <- mapM parseZoneFileZones zones
      let compiled = runFresh $ compileConfig rootServers parsed
      print (fromRules compiled)
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
