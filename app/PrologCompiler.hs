module PrologCompiler (main) where

import Arguments
import DNS.ZoneFile (allRecords, Zone, AZone (zoneName))
import DNS.Wellformedness ( Issue, isFatal, zoneWellformed )
import NameConstraint (toStandardRecord, ConstrainedLabel)
import Parsing
import Resolvable.Consequences (allResolvable, immediateConsequences, toRule)
import Resolvable.Prolog.Core
import Resolvable.Querying (resolve)
import Resolvable.Resolvability
import Resolvable.Update.Disunification (computeRegressions)
import Util.Fresh (runFresh)
import Util.Monad (failLeftS)
import Util

import Control.Monad (when, unless)
import System.IO (hPrint, hPutStrLn, stderr)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
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
      print $ fact () (TFunctor "starting_zone" [nameTerm rootZoneName])
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
      putStrLn $ showKnowledgeBase $ map toRule $ S.toList $ allResolvable rootZoneName res
    "parse-only" -> do
      parsed <- parseZones args
      mapM_ printZone parsed
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
    _ | cmd `elem` ["regressions", "gains"] -> do
        keysPathOld:keysPathNew:rootZ:args' <- return args
        keysOld <- parseDS keysPathOld
        keysNew <- parseDS keysPathNew
        rootZoneName <- parseNameM rootZ
        (old, new) <- sepBy "::" args'
        oldRecs <- parseZones old
        newRecs <- parseZones new
        changes <- failLeftS $ runFresh $ runExceptT $ do
          oldRecsC <- compileConfig rootZoneName keysOld oldRecs
          newRecsC <- compileConfig rootZoneName keysNew newRecs
          oldFacts <- lift $ allResolvable rootZoneName <$> immediateConsequences oldRecsC
          newFacts <- lift $ allResolvable rootZoneName <$> immediateConsequences newRecsC
          let regressions = computeRegressions oldFacts newFacts
          let gains = computeRegressions newFacts oldFacts
          if cmd == "regressions" then regressions else gains
        mapM_ print $ S.map toRule changes
      | otherwise -> fail "unknown command"
  where
    printZone :: Zone ConstrainedLabel -> IO ()
    printZone z = do
      putStrLn $ unwords ["% Zone", show $ zoneName z]
      mapM_ (print . toStandardRecord) $ allRecords z

    parseZones :: [FilePath] -> IO [Zone ConstrainedLabel]
    parseZones paths = do
      parsed <- parseZoneFiles zoneWellformed paths
      mapM_ printIssues (zip paths (map snd parsed))
      when (any (any isFatal . snd) parsed)
        (fail "fatal error encountered")
      return (map fst parsed)

    printIssues :: (FilePath, [Issue String]) -> IO ()
    printIssues (path, issues) = do
      unless (null issues) (hPutStrLn stderr $ unwords ["parsing", path])
      mapM_ (hPrint stderr) issues

    popArg :: [String] -> IO (String, [String])
    popArg [] = fail "too few arguments"
    popArg (cmd:args) = return (cmd, args)
