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
  , endOfEntry
  , sep ) where

import Text.Parsec
import Control.Applicative (asum)
import Control.Monad (when, guard, void)
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
  , _parensStack :: Int }

L.makeLenses ''ParserState

instance OriginProvider ParserState where
  getOrigin = L.view parsedZone

-- For parsers that don't use state
instance OriginProvider () where
  getOrigin _ = Nothing

emptyState :: ParserState
emptyState = ParserState Nothing Nothing Nothing Nothing 0

writeState :: Monad m => L.Lens' ParserState (Maybe a) -> a -> Parser s m ()
writeState l a = modifyState (L.set l (return a))

applyState :: Monad m => L.Lens' ParserState a -> (a -> a) -> Parser s m ()
applyState l = modifyState . L.over l

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
  applyState parensStack (+ 1)

close :: Stream s m Char => Parser s m ()
close = do
  _ <- char ')'
  c <- lensState parensStack
  when (c == 0) (fail "parentheses closed but not opened")
  applyState parensStack (\x -> x - 1)

endOfEntry :: Stream s m Char => Parser s m ()
endOfEntry = do
  c <- lensState parensStack
  if c > 0
    then close
    else void endOfLine <|> eof

hspace' :: Stream s m Char => Parser s m Char
hspace' = hspace <|> do
  c <- lensState parensStack
  guard (c > 0)
  newline

hspaces1 :: Stream s m Char => Parser s m ()
hspaces1 = skipMany1 hspace'

sep :: Stream s m Char => Parser s m ()
sep = asum $ map try [ hspaces1, open, endOfEntry ]
