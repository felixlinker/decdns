module PrologCompiler (main) where

import DNS.ZoneFile (allRecords)
import DNS.Wellformedness ( zoneWellformed )
import NameConstraint (toStandardRecord)
import Resolvable.Prolog.Core
import Resolvable.Resolvability
import Resolvable.Dependencies (computeResolvable, toRule)
import Util.Fresh (runFresh)
import Util.Monad (failLeftS)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Set as S
import Resolvable.Update.Dependencies (computeRegressions)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Parsing.Config (parseZoneFiles)
import DNS.Name (root, prependLabels)
import Resolvable.Querying (resolve)
import Parsing.Name (parseNameM)
import Parsing (parseTypeM)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      keysPath:args' <- return args
      keys <- parseDS keysPath
      (roots, zones) <- sepBy "--" args'
      rootServers <- parseNames roots
      cfg <- parseZones zones
      putStr (showKnowledgeBase $ S.toList $ runFresh $ compileConfig rootServers keys cfg)
    "resolvable" -> do
      keysPath:args' <- return args
      keys <- parseDS keysPath
      (roots, zones) <- sepBy "--" args'
      rootServers <- parseNames roots
      cfg <- parseZones zones
      let res = runFresh $ computeResolvable =<< compileConfig rootServers keys cfg
      putStrLn $ showKnowledgeBase $ map toRule $ S.toList res
    "regressions" -> do
      keysPath:args' <- return args
      keys <- parseDS keysPath
      (sroots, args'') <- sepBy "--" args'
      roots <- parseNames sroots
      (old, new) <- sepBy "--" args''
      oldRecs <- parseZones old
      newRecs <- parseZones new
      regs <- failLeftS $ runFresh $ runExceptT $ do
        oldRecsC <- lift $ compileConfig roots keys oldRecs
        newRecsC <- lift $ compileConfig roots keys newRecs
        computeRegressions oldRecsC newRecsC
      mapM_ print $ S.map toRule regs
    "parse-only" -> do
      parsed <- concatMap (map toStandardRecord . allRecords . snd) <$> parseZones args
      mapM_ print parsed
    "query" -> do
      let rs = [(prependLabels ["a", "root-servers", "net"] root, read "198.41.0.4")]
      [keysPath, t, qn] <- return args
      keys <- parseDS keysPath
      t' <- parseTypeM t
      qn' <- parseNameM qn
      (as, qs) <- resolve keys rs t' qn'
      putStrLn "% Queried:"
      mapM_ (\q -> putStrLn ("% " ++ show q)) (reverse qs)
      when (null as) (putStrLn "no response")
      mapM_ print as
    _ -> fail "unknown command"
  where
    parseZones :: [FilePath] -> IO NameServersConfig
    parseZones = parseZoneFiles zoneWellformed

    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
