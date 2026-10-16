module PrologCompiler (main) where

import DNS.ZoneFile (allRecords, Zone, AZone (zoneName))
import DNS.Wellformedness ( zoneWellformed )
import NameConstraint (toStandardRecord, ConstrainedLabel)
import Resolvable.Prolog.Core
import Resolvable.Resolvability
import Resolvable.Consequences (immediateConsequences, toRule)
import Util.Fresh (runFresh)
import Util.Monad (failLeftS)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Set as S
import Resolvable.Update.Dependencies (computeRegressions, resolvableQuery)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Parsing
import Resolvable.Querying (resolve)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
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
      keysPath:rootZ:args' <- return args
      keys <- parseDS keysPath
      rootZoneName <- parseNameM rootZ
      (old, new) <- sepBy "--" args'
      oldRecs <- parseZones old
      newRecs <- parseZones new
      regs <- failLeftS $ runFresh $ runExceptT $ do
        oldRecsC <- compileConfig rootZoneName keys oldRecs
        newRecsC <- compileConfig rootZoneName keys newRecs
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
      (as, qs, zs) <- resolve keys addrs t' qn'
      putStrLn "% Queried:"
      mapM_ (\q -> putStrLn ("% " ++ show q)) (reverse qs)
      putStrLn "% Data:"
      mapM_ (\z -> do
        putStrLn $ unwords ["% zone:", show (zoneName z)]
        mapM_ (\s -> putStrLn $ "% " ++ show s) (allRecords z))
        zs
      when (null as) (putStrLn "no response")
      mapM_ print as
    _ -> fail "unknown command"
  where
    parseZones :: [FilePath] -> IO [Zone ConstrainedLabel]
    parseZones = parseZoneFiles zoneWellformed

    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
