module PrologCompiler (main) where

import DNS.ZoneFile (allRecords)
import DNS.Diff
import NameConstraint (toStandardRecord, ConstrainedLabel)
import Resolvable.Prolog.Core
import Resolvable.Resolvability
import Resolvable.Update.Resolvability
import Resolvable.Update.Rollout (diffServers, Rollout, rollout)
import Resolvable.Dependencies ( computeResolvable, fromRules, RuleDependencies )
import Util.Fresh (runFresh)
import qualified Util.PartialOrder as PO

import System.Environment (getArgs)
import Util
import Data.List (uncons)
import qualified Data.Map as M
import qualified Data.Set as S
import Resolvable.Update.Dependencies (computeRegressions)
import Control.Monad (unless, when)
import DNS.Name (Name)
import Data.Maybe (isNothing)
import Control.Monad.Trans.Except (runExceptT, runExcept)
import GHC.Base (failIO)
import Parsing (parseNameM, parseTypeM)
import Query (queryStep)

indent :: String -> String
indent s = if null s then "" else "  " ++ s

main :: IO ()
main = do
  (cmd, args) <- popArg =<< getArgs
  case cmd of
    "pl-resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- concat <$> mapM parseZoneFileZones zones
      putStr (showKnowledgeBase $ S.toList $ runFresh $ plResolvable =<< compileConfig rootServers cfg)
    "pl-regressions" -> do
      (roots, args') <- sepBy "--" args
      rootServers <- parseNames roots
      (old, new) <- sepBy "--" args'
      oldCfg <- concat <$> mapM parseZoneFileZones old
      newCfg <- concat <$> mapM parseZoneFileZones new
      putStr (showKnowledgeBase $ S.toList $ plRegressions rootServers oldCfg newCfg)
    "resolvable" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      cfg <- concat <$> mapM parseZoneFileZones zones
      let compiled = runFresh $ plResolvable =<< compileConfig rootServers cfg
      res <- handleError $ runExcept (computeResolvable compiled)
      putStrLn $ showKnowledgeBase $ S.toList res
    "diff" -> do
      (old, new) <- sepBy "--" args
      oldRecs <- concat <$> mapM parseZoneFileZones old
      newRecs <- concat <$> mapM parseZoneFileZones new
      let nsDiff = diffServers oldRecs newRecs
      mapM_ (\(ns, d) -> print ns >> print d) (M.toList nsDiff)
    "regressions" -> do
      (sroots, args') <- sepBy "--" args
      roots <- parseNames sroots
      (old, new) <- sepBy "--" args'
      oldRecs <- concat <$> mapM parseZoneFileZones old
      newRecs <- concat <$> mapM parseZoneFileZones new
      let oldRecsC = compileConfig roots oldRecs
      let newRecsC = compileConfig roots newRecs
      regs <- handleError $ runFresh $ runExceptT $ computeRegressions (plResolvable =<< oldRecsC) (plResolvable =<< newRecsC)
      mapM_ print regs
      let nsDiff = diffServers oldRecs newRecs
      let oldDeps = runFresh $ fromRules <$> oldRecsC
      let newDeps = runFresh $ fromRules <$> newRecsC
      let rolloutSeq = rollout nsDiff oldDeps newDeps
      unless (null rolloutSeq) $ do
        putStrLn $ maybe
          "Update introduces resolution cycle"
          (unlines . ("\n% You can roll out updates as follow.":) . showRollOut)
          rolloutSeq
    "dependencies" -> do
      (roots, zones) <- sepBy "--" args
      rootServers <- parseNames roots
      compiled <- runFresh . compileConfig rootServers . concat <$> mapM parseZoneFileZones zones
      let deps = fromRules compiled :: RuleDependencies
      when (isNothing $ PO.transitiveClosure deps)
        (putStrLn "% Dependencies cyclic")
      print deps
    "parse-only" -> do
      parsed <- concatMap (map toStandardRecord . concatMap allRecords . snd) . concat <$> mapM parseZoneFileZones args
      mapM_ print parsed
    "query" -> do
      (roots, [qnameString, qtypeString]) <- sepBy "--" args
      rootServers <- parseNames roots
      qname <- parseNameM qnameString
      qtype <- parseTypeM qtypeString
      compiled <- compileConfig rootServers <$> parseStdin
      kb <- handleError $ runExcept $ queryStep qname qtype compiled
      putStrLn $ showKnowledgeBase $ S.toList kb
    _ -> fail "unknown command"
  where
    handleError :: Show a => Either a b -> IO b
    handleError = either (failIO . show) return

    popArg :: [String] -> IO (String, [String])
    popArg = maybe (fail "too few arguments") return . uncons

    showRollOut :: Rollout -> [String]
    showRollOut r = map ("% " ++) $ do
      (i, diffs) <- zip [(1 :: Int)..] r
      ("step " ++ show i) : M.foldrWithKey (\ns d -> (showNSDiffs ns d ++)) [] diffs

    showNSDiffs :: Name String -> ZoneDiff ConstrainedLabel -> [String]
    showNSDiffs ns d =
        map indent
      $ ("on server " ++ show ns ++ " roll out:")
        : map indent (lines (show d))
