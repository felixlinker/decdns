module PrologCompiler (main) where

import DNS.ZoneFile (allRecords)
import DNS.Dig (followUpQueries)
import DNS.Wellformedness ( zoneWellformed, responseWellformed )
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
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Parsing (parseNameM, parseTypeM)
import Parsing.Config (parseZoneFiles)
import Query (queryStep)

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
      let res = runFresh $ computeResolvable =<< compileConfig rootServers cfg
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
