{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import GHC.Base (failIO)
import Text.Parsec

import NonDet
import RecursiveResolver
import ZoneFile
import Parsing
import NameConstraint (NameConstraint, compileWildcards, synthesize)

parseZoneFileFile :: FilePath -> IO (Name String, [Record (NameConstraint String)])
parseZoneFileFile path = do
  c <- readFile path
  zf <- parseZoneFile path c >>= handleParser
  ns <- parseName path >>= handleParser
  return (ns, concatMap (allRecords . compileWildcards) zf)
  where
    handleParser :: Either ParseError a -> IO a
    handleParser (Left err) = failIO (show err)
    handleParser (Right a) = return a

runCommands :: M.Map (Name String) [Record (NameConstraint String)] -> IO ()
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
                  go slist)
        ecmd

main :: IO ()
main = do
  files <- getArgs
  zoneFiles <- M.fromList <$> mapM parseZoneFileFile files
  putStrLn "Resolve query with :?"
  runCommands zoneFiles
