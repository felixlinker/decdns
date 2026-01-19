module PrologCompiler (main) where

import Resolvable.Prolog
import Resolvable.Resolvability
import NameConstraint
import ZoneFile

import System.Environment (getArgs)
import Util (parseZoneFileZones)
import Data.List (uncons, partition)

compileZones :: [(Name String, [Zone (NameConstraint String)])] -> [Rule RComp]
compileZones = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule RComp]
    compileOne ns = concatMap (concatMap snd . fromZoneFile ns)

main :: IO ()
main = do
  (cmd, files) <- popArg =<< getArgs
  parsed <- mapM parseZoneFileZones files
  let compiled = compileZones parsed
  case cmd of
    "prolog" -> do
      let (facts, rules) = partition (null . rbody) compiled
      print resolvable
      mapM_ print facts
      mapM_ print rules
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
