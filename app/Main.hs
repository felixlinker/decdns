{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Data.Map as M
import System.Environment (getArgs)
import GHC.Base (failIO, when)
import Text.Parsec (ParseError)

import NonDet
import RecursiveResolver
import ZoneFile

parseZoneFileFile :: FilePath -> IO (Name, [Record])
parseZoneFileFile path = do
  c <- readFile path
  zf <- parseZoneFile path c >>= handleParser
  ns <- parseName path >>= handleParser
  return (ns, zf)
  where
    handleParser :: Either ParseError a -> IO a
    handleParser (Left err) = failIO (show err)
    handleParser (Right a) = return a

main :: IO ()
main = do
  files <- getArgs
  zoneFiles <- M.fromList <$> mapM parseZoneFileFile files
  putStrLn "Resolve query with :?"
  query (uncurry $ resolve (zoneFiles M.!) (M.keys zoneFiles))
  where
    printHelp :: IO ()
    printHelp = putStrLn "could not parse; write :q to exit"

    query :: ((Name, Type) -> NonDet [(Name, Record)]) ->  IO ()
    query r = do
      l <- getLine
      ecmd <- parseCommand l
      either
        (\_ -> printHelp >> query r)
        (\case  Exit -> return ()
                Query q -> do
                  let traces = NonDet.all (r q)
                  when (null traces) (putStrLn "query cannot be resolved")
                  mapM_ print traces
                  query r)
        ecmd
