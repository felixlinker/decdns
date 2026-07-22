{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Parsing.ZoneFile
  ( ServedZone
  , parseRecords
  , parseZoneFile ) where

import Text.Parsec
import Control.Applicative (asum)
import Data.Functor (($>))

import Parsing.State
import Parsing.Tokens
import Parsing.Name
import Parsing.Data
import DNS.Name (Name, Named(Named))
import DNS.Record (StandardRecord, RData(..), Record)
import Parsing.Dig
import Data.Maybe (catMaybes)

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

nsHintOrLine :: Stream s m Char => Parser s m (Maybe (Either StandardRecord (Name String)))
nsHintOrLine = choice [right (try received), right (try served), left line]
  where
    left :: Monad m => m (Maybe a) -> m (Maybe (Either a b))
    left = fmap (fmap Left)

    right :: Monad m => m b -> m (Maybe (Either a b))
    right = fmap (Just . Right)

type ServedZone = (Name String, [StandardRecord])

zone :: Stream s m Char => Parser s m ServedZone
zone = aux []
  where
    aux :: Stream s m Char => [StandardRecord] -> Parser s m ServedZone
    aux acc = do
      l <- nsHintOrLine
      maybe (aux acc) (either (aux . (:acc)) (return . (,acc))) l

records :: Stream s m Char => Parser s m [StandardRecord]
records = catMaybes <$> many line

zoneFile :: Stream s m Char => Parser s m [ServedZone]
zoneFile = many zone

parseRecords :: Stream s m Char => SourceName -> s -> m (Either ParseError [StandardRecord])
parseRecords = runParserT (records <* eof) emptyState

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [ServedZone])
parseZoneFile = runParserT (zoneFile <* eof) emptyState
