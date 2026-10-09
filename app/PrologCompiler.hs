module PrologCompiler (main) where

import DNS.ZoneFile (allRecords, mergeZones)
import DNS.Dig (followUpQueries)
import DNS.Wellformedness ( zoneWellformed, responseWellformed )
import NameConstraint (toStandardRecord)
import Resolvable.Prolog.Core
import Resolvable.Resolvability
import Resolvable.Update.Rollout (diffServers, showRollOut, rollout, fromResolutionOrder)
import Resolvable.Dependencies (computeResolvable, toRule, resolutionOrder)
import Util.Fresh (runFresh)
import Util.Monad (failLeftS, handleErrorS, handleMaybe)

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Map as M
import qualified Data.Set as S
import Resolvable.Update.Dependencies (computeRegressions)
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Parsing (parseNameM, parseTypeM)
import Parsing.Config (parseZoneFiles)
import Query (queryStep)
import Visualizer (renderVisualization)

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      putStr (showKnowledgeBase $ S.toList $ runFresh $ compileConfig rootServers cfg)
    "resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      let res = runFresh $ computeResolvable True =<< compileConfig rootServers cfg
      putStrLn $ showKnowledgeBase $ map toRule $ S.toList res
    "regressions" -> do
      (sroots, args') <- sepBy "--" args
      roots <- parseNames sroots
      (old, new) <- sepBy "--" args'
      oldRecs <- parseZones old
      newRecs <- parseZones new
      regs <- failLeftS $ runFresh $ runExceptT $ do
        oldRecsC <- lift $ compileConfig roots oldRecs
        newRecsC <- lift $ compileConfig roots newRecs
        computeRegressions oldRecsC newRecsC
      mapM_ print $ S.map toRule regs
    "visualize" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- parseZones zones
      let compiled = runFresh $ compileConfig rootServers cfg
      putStrLn $ renderVisualization rootServers zones cfg compiled
    "parse-only" -> do
      parsed <- concatMap (map toStandardRecord . allRecords . snd) <$> parseZones args
      mapM_ print parsed
    "query" -> do
      (roots, [qnameString, qtypeString]) <- sepBy "--" args
      rootServers <- parseNames roots
      qname <- parseNameM qnameString
      qtype <- parseTypeM qtypeString
      cfg <- parseResponse
      let kb = runFresh (queryStep qname qtype =<< compileConfig rootServers cfg)
      let answers = filter isFact $ S.toList kb
      if null answers
        then mapM_ print $ followUpQueries kb
        else putStrLn $ showKnowledgeBase answers
    _ -> fail "unknown command"
  where
    parseZones :: [FilePath] -> IO NameServersConfig
    parseZones = parseZoneFiles zoneWellformed

    parseResponse :: IO NameServersConfig
    parseResponse = parseStdin responseWellformed

    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons
