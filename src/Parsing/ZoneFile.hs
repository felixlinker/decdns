{-# LANGUAGE FlexibleContexts #-}
module Parsing.ZoneFile
  ( parseZone
  , parseZoneFile ) where

import Text.Parsec
import Text.Parsec.Char as Char
import Control.Applicative (asum)
import Data.Functor (($>), void)
import Data.Maybe (catMaybes)

import Parsing.State
import Parsing.Tokens
import Parsing.Name
import Parsing.Data
import DNS.ZoneFile
import DNS.Name (Name, Named(Named))
import DNS.Record (StandardRecord, RData(..), Record)

comment :: Stream s m Char => Parser s m ()
comment = do
  spaces
  _ <- Char.char ';'
  _ <- manyTill anyChar (void endOfLine <|> eof)
  return ()

origin :: Stream s m Char => Parser s m (Name String)
origin = do
  spaces
  _ <- string "$ORIGIN"
  sep
  org <- name
  writeState parsedZone org
  return org

setTTL :: Stream s m Char => Parser s m ()
setTTL = do
  spaces
  _ <- string "$TTL"
  sep
  t <- int
  writeState lastTTL t

record :: Stream s m Char => Parser s m StandardRecord
record = try catTtlType <|> try nameCatTtlType <|> nameTtlCatType

-- This parser is required because the category can be parsed as label. We
-- prioritize that parsing over parsing it as name for correctness in more
-- cases.
catTtlType :: Stream s m Char => Parser s m (Record String)
catTtlType = do
  spaces
  n <- readState lastName
  spaces
  c <- tryOrState category lastCat
  spaces
  t <- tryOrState int lastTTL
  spaces
  ty <- recordTypeData
  sep
  return $ Named n (RData c t ty)

nameCatTtlType :: Stream s m Char => Parser s m (Record String)
nameCatTtlType = do
  spaces
  n <- tryOrState name lastName
  spaces
  c <- tryOrState category lastCat
  spaces
  t <- tryOrState int lastTTL
  spaces
  ty <- recordTypeData
  sep
  return $ Named n (RData c t ty)

nameTtlCatType :: Stream s m Char => Parser s m (Record String)
nameTtlCatType = do
  spaces
  n <- tryOrState name lastName
  spaces
  t <- tryOrState int lastTTL
  spaces
  c <- tryOrState category lastCat
  spaces
  ty <- recordTypeData
  sep
  return $ Named n (RData c t ty)

line :: Stream s m Char => Parser s m (Maybe StandardRecord)
line = do
  spaces
  r <- asum $ map try
      [ comment $> Nothing
      , setTTL $> Nothing
      , Just <$> record ]
  spaces
  return r

zone :: Stream s m Char => Parser s m (Zone String)
zone = do
  org <- origin
  rs <- catMaybes <$> many line
  return $ foldl insert (emptyZone org) rs

zoneFile :: Stream s m Char => Parser s m [Zone String]
zoneFile = many zone <* eof

parseZone :: Stream s m Char => SourceName -> s -> m (Either ParseError (Zone String))
parseZone = runParserT zone emptyState

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Zone String])
parseZoneFile = runParserT zoneFile emptyState
