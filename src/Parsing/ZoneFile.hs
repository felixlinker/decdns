{-# LANGUAGE FlexibleContexts #-}
module Parsing.ZoneFile
  ( parseZoneFile ) where

import Text.Parsec
import Text.Parsec.Char as Char
import Text.Read (readMaybe)
import Control.Applicative (asum)
import Data.Functor (($>))
import Data.Maybe (catMaybes)

import Parsing.State
import Parsing.Tokens
import Parsing.Name
import ZoneFile
import Parsing.Wellformedness (wellformed)

comment :: Stream s m Char => Parser s m ()
comment = do
  spaces
  _ <- Char.char ';'
  _ <- manyTill anyChar (endOfLine $> () <|> eof)
  return ()

origin :: Stream s m Char => Parser s m (Name String)
origin = do
  spaces
  _ <- string "$ORIGIN"
  spaces1
  org <- name
  writeState parsedZone org
  return org

ttl :: Stream s m Char => Parser s m Int
ttl = do
  ttls <- many1 digit
  maybe (fail "cannot parse") return (readMaybe ttls)

setTTL :: Stream s m Char => Parser s m ()
setTTL = do
  _ <- string "$TTL"
  spaces1
  t <- ttl
  writeState lastTTL t

record :: Stream s m Char => Parser s m (Record (Name String))
record = do
  spaces
  n <- tryOrState name lastName
  spaces
  (c, t, ty) <- try catTtlType <|> try ttlCatType
  spaces1
  d <- manyTill anyChar end
  return (Record n (RData c t ty (words d)))
  where
    end :: Stream s m Char => Parser s m ()
    end = endOfLine $> () <|> eof

catTtlType :: Stream s m Char => Parser s m (Cat, Int, Type)
catTtlType = do
  c <- tryOrState category lastCat
  spaces
  t <- tryOrState ttl lastTTL
  spaces1
  ty <- recordType
  return (c, t, ty)

ttlCatType :: Stream s m Char => Parser s m (Cat, Int, Type)
ttlCatType = do
  t <- tryOrState ttl lastTTL
  spaces
  c <- tryOrState category lastCat
  spaces1
  ty <- recordType
  return (c, t, ty)

line :: Stream s m Char => Parser s m (Maybe (Record (Name String)))
line = spaces >> asum (map try
  [ comment $> Nothing
  , setTTL $> Nothing
  , Just <$> record ])

zone :: Stream s m Char => Parser s m (Zone String)
zone = do
  org <- origin
  rs <- catMaybes <$> many line
  z <- foldl safeInsert (return (emptyZone org)) rs
  maybe (return z) (\msg -> fail ("zone ill-formed: " ++ msg)) (wellformed z)
  where
    safeInsert :: MonadFail m => m (Zone String) -> Record (Name String) -> m (Zone String)
    safeInsert zm r = do
      z <- zm
      maybe (fail "cannot insert record - illformed zone") return (insert z r)

zoneFile :: Stream s m Char => Parser s m [Zone String]
zoneFile = many zone

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Zone String])
parseZoneFile = runParserT zoneFile emptyState
