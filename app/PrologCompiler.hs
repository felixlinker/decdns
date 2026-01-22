module PrologCompiler (main) where

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Dependencies
import NameConstraint
import ZoneFile

import System.Environment (getArgs)
import Util
import Data.List (uncons, partition)

compileZones :: [Name String] -> [(Name String, [Zone (NameConstraint String)])] -> [Rule RComp]
compileZones rootServers = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule RComp]
    compileOne ns = concatMap (fromZoneFile rootServers ns)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "prolog" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      parsed <- mapM parseZoneFileZones zones
      let compiled = compileZones rootServers parsed
      let (facts, rules) = partition (null . rbody) compiled
      print resolvable
      mapM_ print facts
      mapM_ print rules
    "delta" -> do
      (roots, args') <- sepBy "--" args
      (old, new) <- sepBy "--" args'
      rootServers <- parseNames roots
      rold <- renameResolves "resolves_old" . compileZones rootServers <$> mapM parseZoneFileZones old
      rnew <- renameResolves "resolves_new" . compileZones rootServers <$> mapM parseZoneFileZones new
      print regression
      mapM_ print rold
      mapM_ print rnew
    "deps" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      parsed <- mapM parseZoneFileZones zones
      let compiled = compileZones rootServers parsed
      print (fromRules compiled)
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons

    renameResolves :: String -> [Rule a] -> [Rule a]
    renameResolves to = map (mapRule (renamePred "resolves" to))
