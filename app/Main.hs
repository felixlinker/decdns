{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor (($>))
import Control.Applicative (asum)
import System.Environment (getArgs)
import GHC.Base (failIO)
import Text.Parsec

import NonDet
import RecursiveResolver
import ZoneFile

parseZoneFileFile :: FilePath -> IO (Name, [Record])
parseZoneFileFile path = do
  c <- readFile path
  zf <- parseZoneFile path c >>= handleParser
  ns <- parseName path >>= handleParser
  return (ns, concatMap records zf)
  where
    handleParser :: Either ParseError a -> IO a
    handleParser (Left err) = failIO (show err)
    handleParser (Right a) = return a

type Query = (Name, Type)

data Command = Exit | Query Query | SList SList
  deriving Show

query :: Stream s m Char => Parser s m Command
query = do
  _ <- string ":?"
  spaces
  n <- name
  spaces
  t <- recordType
  return $ Query (n, t)

nslist :: Stream s m Char => Parser s m Command
nslist = do
  _ <- string ":slist"
  SList <$> many (spaces >> name)

exit :: Stream s m Char => Parser s m Command
exit = string ":q" $> Exit

commands :: Stream s m Char => Parser s m Command
commands = asum $ map try [ query, nslist, exit ]

parseCommand :: Monad m => String -> m (Either ParseError Command)
parseCommand = runParserT commands emptyState "parseCommand function"

runCommands :: M.Map Name [Record] -> IO ()
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
                  let traces = S.fromList $ NonDet.all (resolve (nses M.!) slist qname qtype)
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
