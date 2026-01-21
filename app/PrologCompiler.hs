module PrologCompiler (main) where

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.Dependencies
import NameConstraint
import ZoneFile

import System.Environment (getArgs)
import Util (parseZoneFileZones)
import Data.List (uncons, partition)
import GHC.Base (failIO)
import Control.Monad (guard)

compileZones :: [(Name String, [Zone (NameConstraint String)])] -> [Rule RComp]
compileZones = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule RComp]
    compileOne ns = concatMap (fromZoneFile ns)

main :: IO ()
main = do
  (cmd, rest) <- popArg =<< getArgs
  case cmd of
    "prolog" -> do
      parsed <- mapM parseZoneFileZones rest
      let compiled = compileZones parsed
      let (facts, rules) = partition (null . rbody) compiled
      print resolvable
      mapM_ print facts
      mapM_ print rules
    "delta" -> do
      let (old, tmp) = span (/= "--") rest
      (sep, new) <- maybe (failIO "missing separator") return $ uncons tmp
      guard (sep == "--")
      rold <- renameResolves "resolves_old" . compileZones <$> mapM parseZoneFileZones old
      rnew <- renameResolves "resolves_new" . compileZones <$> mapM parseZoneFileZones new
      print regression
      mapM_ print rold
      mapM_ print rnew
    "deps" -> do
      parsed <- mapM parseZoneFileZones rest
      let compiled = compileZones parsed
      print (fromRules compiled)
    _ -> fail "unknown command"
  where
    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons

    renameResolves :: String -> [Rule a] -> [Rule a]
    renameResolves to = map (mapRule (renamePred "resolves" to))
