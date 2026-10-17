module PrologCompiler (main) where

import Arguments
import DNS.ZoneFile (allRecords, Zone)
import DNS.Wellformedness ( zoneWellformed )
import NameConstraint (toStandardRecord, ConstrainedLabel)
import Parsing
import Resolvable.Consequences (immediateConsequences, toRule)
import Resolvable.Prolog.Core
import Resolvable.Querying (resolve)
import Resolvable.Resolvability
import Resolvable.Update.Dependencies (computeRegressions, resolvableQuery)
import Util.Fresh (runFresh)
import Util.Monad (failLeftS)
import Util

import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import System.Environment (getArgs)
import qualified System.Timeout as Sys

main :: IO ()
main = do
  (_options, commandArgs) <- parseArguments =<< getArgs
  (cmd, args) <- popArg commandArgs
  case cmd of
    "pl-resolvable" -> do
      keysPath:rootZ:zones <- return args
      keys <- parseDS keysPath
      rootZoneName <- parseNameM rootZ
      cfg <- parseZones zones
      rules <- failLeftS $ runFresh $ runExceptT $ compileConfig rootZoneName keys cfg
      putStr (showKnowledgeBase $ S.toList rules)
    "resolves" -> do
      keysPath:rootZ:zones <- return args
      keys <- parseDS keysPath
      rootZoneName <- parseNameM rootZ
      cfg <- parseZones zones
      res <- failLeftS $ runFresh $ runExceptT $ compileConfig rootZoneName keys cfg >>= lift . immediateConsequences
      putStrLn $ showKnowledgeBase $ map toRule $ S.toList res
    "resolvable" -> do
      keysPath:rootZ:zones <- return args
      keys <- parseDS keysPath
      rootZoneName <- parseNameM rootZ
      cfg <- parseZones zones
      res <- failLeftS $ runFresh $ runExceptT $ compileConfig rootZoneName keys cfg >>= lift . immediateConsequences
      putStrLn $ showKnowledgeBase $ mapMaybe (fmap toRule . resolvableQuery) $ S.toList res
    "regressions" -> do
      keysPathOld:keysPathNew:rootZ:args' <- return args
      keysOld <- parseDS keysPathOld
      keysNew <- parseDS keysPathNew
      rootZoneName <- parseNameM rootZ
      (old, new) <- sepBy "::" args'
      oldRecs <- parseZones old
      newRecs <- parseZones new
      regs <- failLeftS $ runFresh $ runExceptT $ do
        oldRecsC <- compileConfig rootZoneName keysOld oldRecs
        newRecsC <- compileConfig rootZoneName keysNew newRecs
        computeRegressions oldRecsC newRecsC
      mapM_ print $ S.map toRule regs
    "parse-only" -> do
      parsed <- concatMap (map toStandardRecord . allRecords) <$> parseZones args
      mapM_ print parsed
    "query" -> do
      [addrsPath, keysPath, t, qn] <- return args
      addrs <- parseAddrs addrsPath
      keys <- parseDS keysPath
      t' <- parseTypeM t
      qn' <- parseNameM qn
      let time = if timeout _options <= 0
          then fmap Just
          else Sys.timeout (timeout _options * (10 :: Int) Prelude.^ (6 :: Int))
      (as, _, _) <- maybe (fail "timeout") return =<< time (resolve keys addrs t' qn')
      when (null as) (putStrLn "no response")
      mapM_ print as
    _ -> fail "unknown command"
  where
    parseZones :: [FilePath] -> IO [Zone ConstrainedLabel]
    parseZones = parseZoneFiles zoneWellformed

    popArg :: [String] -> IO (String, [String])
    popArg [] = fail "too few arguments"
    popArg (cmd:args) = return (cmd, args)
