module PrologCompiler (main) where

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Dependencies
import NameConstraint
import ZoneFile
import Parsing (parseNameM)

import System.Environment (getArgs)
import Util (parseZoneFileZones)
import Control.Monad (void)
import Data.List (uncons, partition)

compileZones :: Name String -> [(Name String, [Zone (NameConstraint String)])] -> [Rule RComp]
compileZones root = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule RComp]
    compileOne ns = concatMap (concatMap snd . fromZoneFile root ns)

main :: IO ()
main = do
  (root, remaining) <- popArg =<< getArgs
  (cmd, files) <- popArg remaining
  rootName <- parseNameM root
  parsed <- mapM parseZoneFileZones files
  let compiled = compileZones rootName parsed
  case cmd of
    "prolog" -> do
      let (facts, rules) = partition (null . rbody) compiled
      print (resolvable rootName)
      mapM_ print facts
      mapM_ print rules
    "deps" -> print (fromRules compiled)
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
