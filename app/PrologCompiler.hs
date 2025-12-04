module PrologCompiler (main) where

import Resolvable.Prolog (Rule, fromZoneFile)
import System.Environment (getArgs)

import NameConstraint (NameConstraint)
import Util (parseZoneFileZones)
import Util.Fresh (evalFresh)
import ZoneFile (Name, Zone)

compileZones :: [(Name String, [Zone (NameConstraint String)])] -> [Rule]
compileZones parsed = evalFresh $ concat <$> mapM compileOne parsed
  where
    compileOne (ns, zs) = concat <$> mapM (fromZoneFile ns) zs

main :: IO ()
main = do
  files <- getArgs
  parsed <- mapM parseZoneFileZones files
  mapM_ print (compileZones parsed)
