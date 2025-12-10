module PrologCompiler (main) where

import Resolvable.Prolog (Rule (rbody), fromZoneFile)
import System.Environment (getArgs)

import NameConstraint (NameConstraint)
import Util (parseZoneFileZones)
import ZoneFile (Name, Zone)

compileZones :: [(Name String, [Zone (NameConstraint String)])] -> [Rule]
compileZones = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule]
    compileOne ns = concatMap (fromZoneFile ns)

spanAll :: (a -> Bool) -> [a] -> ([a], [a])
spanAll p l = ([ x | x <- l, p x ], [ x | x <- l, not (p x) ])

main :: IO ()
main = do
  files <- getArgs
  parsed <- mapM parseZoneFileZones files
  let (facts, rules) = spanAll (null . rbody) (compileZones parsed)
  mapM_ print facts
  mapM_ print rules
