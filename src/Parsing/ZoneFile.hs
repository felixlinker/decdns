{-# LANGUAGE FlexibleContexts #-}
module Parsing.ZoneFile
  ( parseRecords
  , parseZoneFiles
  , parseDS ) where

import Text.Parsec
import Control.Applicative (asum)
import Control.Monad (when, unless)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import System.IO (hPrint, stderr)

import Parsing.State
import Parsing.Tokens
import Parsing.Name
import Parsing.Data
import NameConstraint (ConstrainedLabel, compileWildcards)
import DNS.Name (Name, Named(Named))
import DNS.Record (StandardRecord, RData(..), Record)
import qualified DNS.RecordType as T
import DNS.RRSet (RRSet, rrsetType)
import DNS.Wellformedness (Issue, isFatal)
import DNS.ZoneFile (Zone, fromRecordList, allRRSets)
import Util.Parsing (handleParser)
import Util.Monad (handleMaybe)

origin :: Stream s m Char => Parser s m (Name String)
origin = do
  skipSep
  _ <- string "$ORIGIN"
  sep
  org <- name
  endOfEntry
  writeState parsedZone org
  return org

setTTL :: Stream s m Char => Parser s m ()
setTTL = do
  skipSep
  _ <- string "$TTL"
  sep
  t <- int
  endOfEntry
  writeState lastTTL t

record :: Stream s m Char => Parser s m StandardRecord
record = try catTtlType <|> try ttlCatType <|> try nameCatTtlType <|> nameTtlCatType

-- This parser is required because the category can be parsed as label. We
-- prioritize that parsing over parsing it as name for correctness in more
-- cases.
catTtlType :: Stream s m Char => Parser s m (Record String)
catTtlType = do
  skipSep
  n <- readState lastName
  skipSep
  c <- tryOrState category lastCat
  skipSep
  t <- tryOrState int lastTTL
  skipSep
  ty <- recordTypeData
  endOfEntry
  return $ Named n (RData c t ty)

ttlCatType :: Stream s m Char => Parser s m (Record String)
ttlCatType = do
  skipSep
  n <- readState lastName
  t <- int
  writeState lastTTL t
  skipSep
  c <- tryOrState category lastCat
  skipSep
  ty <- recordTypeData
  endOfEntry
  return $ Named n (RData c t ty)

nameCatTtlType :: Stream s m Char => Parser s m (Record String)
nameCatTtlType = do
  skipSep
  n <- tryOrState name lastName
  skipSep
  c <- tryOrState category lastCat
  skipSep
  t <- tryOrState int lastTTL
  skipSep
  ty <- recordTypeData
  endOfEntry
  return $ Named n (RData c t ty)

nameTtlCatType :: Stream s m Char => Parser s m (Record String)
nameTtlCatType = do
  skipSep
  n <- tryOrState name lastName
  skipSep
  t <- tryOrState int lastTTL
  skipSep
  c <- tryOrState category lastCat
  skipSep
  ty <- recordTypeData
  endOfEntry
  return $ Named n (RData c t ty)

line :: Stream s m Char => Parser s m (Maybe StandardRecord)
line = asum $ map try
  [ skipMany hspace >> comment $> Nothing
  , skipMany hspace >> eol $> Nothing
  , origin $> Nothing
  , setTTL $> Nothing
  , Just <$> record ]

records :: Stream s m Char => Parser s m [StandardRecord]
records = catMaybes <$> many line

parseRecords :: Stream s m Char => SourceName -> s -> m (Either ParseError [StandardRecord])
parseRecords = runParserT (records <* eof) emptyState

parseZoneFileContents :: (Zone String -> [Issue String]) -> SourceName -> FilePath -> IO (Zone ConstrainedLabel)
parseZoneFileContents wfc source path = do
  content <- readFile path
  recs <- handleParser =<< parseRecords source content
  z <- handleMaybe "empty zone file" $ fromRecordList <$> NE.nonEmpty recs
  wellformed z
  return $ compileWildcards z
  where
    wellformed :: Zone String -> IO ()
    wellformed zone = do
      let issues = wfc zone
      unless (null issues) (putStrLn $ unwords ["parsing", path])
      mapM_ (hPrint stderr) issues
      when (any isFatal issues) (fail "fatal error encountered")

parseZoneFiles :: (Zone String -> [Issue String]) -> [FilePath] -> IO [Zone ConstrainedLabel]
parseZoneFiles wfc = mapM (\path -> parseZoneFileContents wfc path path)

parseDS :: FilePath -> IO [RRSet String]
parseDS path = do
  recs <- handleParser =<< parseRecords path =<< readFile path
  return $ maybe [] (filter ((== T.DS) . rrsetType) . allRRSets . fromRecordList) (NE.nonEmpty recs)
