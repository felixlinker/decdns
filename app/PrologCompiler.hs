module PrologCompiler (main) where

import Resolvable.Prolog (Rule (rbody))
import Resolvable.Resolvability (resolvable, fromZoneFile)
import System.Environment (getArgs)

import NameConstraint (NameConstraint)
import Util (parseZoneFileZones)
import ZoneFile (Name, Zone)
import Data.List (uncons)
import Parsing (parseNameM)

compileZones :: Name String -> [(Name String, [Zone (NameConstraint String)])] -> [Rule ()]
compileZones root = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule ()]
    compileOne ns = concatMap (concatMap snd . fromZoneFile root ns)

spanAll :: (a -> Bool) -> [a] -> ([a], [a])
spanAll p l = ([ x | x <- l, p x ], [ x | x <- l, not (p x) ])

main :: IO ()
main = do
  (root, files) <- maybe (fail "too few arguments") return . uncons =<< getArgs
  rootName <- parseNameM root
  parsed <- mapM parseZoneFileZones files
  let (facts, rules) = spanAll (null . rbody) (compileZones rootName parsed)
  print (resolvable rootName)
  mapM_ print facts
  mapM_ print rules
