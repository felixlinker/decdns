{-# LANGUAGE FlexibleContexts #-}
module Parsing.ZoneFile
  ( parseZoneFile ) where

import Text.Parsec
import Control.Applicative (asum)
import Data.Functor (($>))
import Data.Maybe (catMaybes)

import Parsing.State
import Parsing.Tokens
import Parsing.Name
import Parsing.Data
import DNS.Name (Name, Named(Named))
import DNS.Record (StandardRecord, RData(..), Record)

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
record = try catTtlType <|> try nameCatTtlType <|> nameTtlCatType

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
line = do
  skipSepNewLine -- to skip leading comments and new lines
  asum $ map try
    [ origin $> Nothing
    , setTTL $> Nothing
    , Just <$> record ]

zoneFile :: Stream s m Char => Parser s m [StandardRecord]
zoneFile = catMaybes <$> many line

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [StandardRecord])
parseZoneFile = runParserT (zoneFile <* eof) emptyState
