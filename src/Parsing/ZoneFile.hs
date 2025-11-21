{-# LANGUAGE FlexibleContexts #-}
module Parsing.ZoneFile
  ( parseZoneFile ) where

import Text.Parsec
import Text.Parsec.Char as Char
import Text.Read (readMaybe)
import Control.Applicative (asum)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Map as M

import Parsing.State
import Parsing.Tokens
import Parsing.Name
import ZoneFile

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
  name

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

record :: Stream s m Char => Parser s m (Record String)
record = do
  spaces
  n <- tryOrState relName lastName
  spaces
  (c, t, ty) <- try catTtlType <|> try ttlCatType
  spaces1
  d <- manyTill anyChar end
  return (Record n c t ty (words d))
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

line :: Stream s m Char => Parser s m (Maybe (Record String))
line = spaces >> asum (map try
  [ comment $> Nothing
  , setTTL $> Nothing
  , Just <$> record ])

zone :: Stream s m Char => Parser s m Zone
zone = do
  org <- origin
  rs <- catMaybes <$> many line
  return $ foldl insert (Zone org [] M.empty) rs

zoneFile :: Stream s m Char => Parser s m [Zone]
zoneFile = many zone

parseZoneFile :: Stream s m Char => SourceName -> s -> m (Either ParseError [Zone])
parseZoneFile = runParserT zoneFile emptyState
