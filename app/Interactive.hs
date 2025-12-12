{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Interactive (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)

import NonDet
import RecursiveResolver
import ZoneFile
import Parsing
import NameConstraint
import Util (parseZoneFileZones)
import Control.Monad (forM_)

parseZoneFileFile :: FilePath -> IO (Name String, [Record ConstrainedName])
parseZoneFileFile path = do
  (ns, zfs) <- parseZoneFileZones path
  return (ns, concatMap allRecords zfs)

rToName :: Record ConstrainedName -> Record (Name String)
rToName (Record n d) = Record (toName n) d

printNameServer :: Name String -> [Record ConstrainedName] -> IO ()
printNameServer n (map rToName -> rs) = do
  putStrLn $ "name server " ++ show n ++ " serves..."
  forM_ rs (\r -> do
    putStr "  "
    print r)

runCommands :: M.Map (Name String) [Record ConstrainedName] -> IO ()
runCommands nses = go (M.keys nses)
  where
    printHelp :: IO ()
    printHelp = putStrLn "could not parse; write :q to exit"

    go :: SList -> IO ()
    go slist = do
      l <- getLine
      ecmd <- parseCommand l
      either
        (\_ -> printHelp >> go slist)
        (\case  Exit -> return ()
                SList slist' -> go (filter (`S.member` M.keysSet nses) slist')
                Query (qname, qtype) -> do
                  let queryF ns qn = map (synthesize qn) (nses M.! ns)
                  let traces = S.fromList $ NonDet.all (resolve queryF slist qname qtype)
                  let minimal t = not $ any (`subtrace` t) traces
                  let traces' =  S.filter minimal traces
                  putStrLn (if S.null traces' then "query cannot be resolved" else show (S.size traces') ++ " results")
                  mapM_ print traces'
                  go slist
                Print -> do
                  forM_ (M.toList nses) (uncurry printNameServer)
                  go slist)
        ecmd

main :: IO ()
main = do
  files <- getArgs
  zoneFiles <- M.fromList <$> mapM parseZoneFileFile files
  putStrLn "Resolve query with :?"
  runCommands zoneFiles
