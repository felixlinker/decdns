{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Parsing.State
  ( Parser
  , ParserState
  , OriginProvider(..)
  , parsedZone
  , lastName
  , lastTTL
  , lastCat
  , emptyState
  , readState
  , writeState
  , tryOrState
  , comment
  , endOfEntry
  , skipSep
  , skipSepNewLine
  , sep ) where

import Text.Parsec
import Control.Monad (when, guard, void, unless)
import qualified Control.Lens as L

import DNS.Name
import DNS.Record
import Parsing.Tokens

type Parser s m = ParsecT s ParserState m

class OriginProvider a where
  getOrigin :: a -> Maybe (Name String)

data ParserState = ParserState
  { _parsedZone :: Maybe (Name String)
  , _lastName :: Maybe (Name String)
  , _lastTTL :: Maybe Int
  , _lastCat :: Maybe Cat
  , _parens :: Bool }

L.makeLenses ''ParserState

instance OriginProvider ParserState where
  getOrigin = L.view parsedZone

-- For parsers that don't use state
instance OriginProvider () where
  getOrigin _ = Nothing

emptyState :: ParserState
emptyState = ParserState Nothing Nothing Nothing Nothing False

setLensState :: Monad m => L.Lens' ParserState a -> a -> Parser s m ()
setLensState l a = modifyState (L.set l a)

writeState :: Monad m => L.Lens' ParserState (Maybe a) -> a -> Parser s m ()
writeState l = setLensState l . return

lensState :: Monad m => L.Lens' ParserState a -> Parser s m a
lensState l = L.view l <$> getState

readState :: Monad m => L.Lens' ParserState (Maybe a) -> Parser s m a
readState l = do
  ma <- lensState l
  maybe (fail "required state missing") return ma

tryOrState :: Monad m => Parser s m a -> L.Lens' ParserState (Maybe a) -> Parser s m a
tryOrState p l = tryP <|> readState l
  where
    tryP = do
      a <- try p
      writeState l a
      return a

open :: Stream s m Char => Parser s m ()
open = do
  _ <- char '('
  p <- lensState parens
  when p (fail "do not nest parentheses")
  setLensState parens True

close :: Stream s m Char => Parser s m ()
close = do
  _ <- char ')'
  p <- lensState parens
  unless p (fail "parentheses closed but not opened")
  setLensState parens False

hspace' :: Stream s m Char => Bool -> Parser s m ()
hspace' acceptNl = do
  inParens <- lensState parens
  if acceptNl || inParens then eol <|> hspace else hspace

comment :: Stream s m Char => Parser s m ()
comment = do
  _ <- char ';'
  void $ manyTill anyChar (try eol)

sepOne :: Stream s m Char => Bool -> Parser s m ()
sepOne acceptNl = open <|> close <|> comment <|> hspace' acceptNl

skipSep :: Stream s m Char => Parser s m ()
skipSep = skipMany (sepOne False)

skipSepNewLine :: Stream s m Char => Parser s m ()
skipSepNewLine = skipMany (sepOne True)

sep :: Stream s m Char => Parser s m ()
sep = skipMany1 (sepOne False)

endOfEntry :: Stream s m Char => Parser s m ()
endOfEntry = do
  skipSepNewLine
  inParens <- lensState parens
  guard (not inParens)
