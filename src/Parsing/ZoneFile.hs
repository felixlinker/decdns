{-# LANGUAGE FlexibleContexts #-}
module Parsing.ZoneFile
  ( parseZoneFile ) where

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
import DNS.Record (StandardRecord, Cat, RData(..))

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
record = do
  spaces
  n <- tryOrState name lastName
  spaces
  (c, t, ty) <- try catTtlType <|> try ttlCatType
  sep
  Named n . RData c t <$> ty

catTtlType :: Stream s m Char => Parser s m (Cat, Int, TypeParser s m)
catTtlType = do
  c <- tryOrState category lastCat
  spaces
  t <- tryOrState int lastTTL
  sep
  ty <- recordTypeData
  return (c, t, ty)

ttlCatType :: Stream s m Char => Parser s m (Cat, Int, TypeParser s m)
ttlCatType = do
  t <- tryOrState int lastTTL
  spaces
  c <- tryOrState category lastCat
  sep
  ty <- recordTypeData
  return (c, t, ty)

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
zoneFile = many zone

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Zone String])
parseZoneFile = runParserT zoneFile emptyState
