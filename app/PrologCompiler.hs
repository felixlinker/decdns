module PrologCompiler (main) where

import Resolvable.Prolog (Rule, fromZoneFile)
import System.Environment (getArgs)

import NameConstraint (NameConstraint)
import Util (parseZoneFileZones)
import ZoneFile (Name, Zone)

compileZones :: [(Name String, [Zone (NameConstraint String)])] -> [Rule]
compileZones = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule]
    compileOne ns = concatMap (fromZoneFile ns)

main :: IO ()
main = do
  files <- getArgs
  parsed <- mapM parseZoneFileZones files
  mapM_ print (compileZones parsed)
