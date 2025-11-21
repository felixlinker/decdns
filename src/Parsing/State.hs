{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module Parsing.State
  ( Parser
  , ParserState
  , lastName
  , lastTTL
  , lastCat
  , emptyState
  , readState
  , writeState
  , tryOrState ) where

import Text.Parsec
import qualified Control.Lens as L

import ZoneFile

type Parser s m = ParsecT s ParserState m

data ParserState = ParserState
  { _lastName :: Maybe (Name String)
  , _lastTTL :: Maybe Int
  , _lastCat :: Maybe Cat }

L.makeLenses ''ParserState

emptyState :: ParserState
emptyState = ParserState Nothing Nothing Nothing

writeState :: Monad m => L.Lens' ParserState (Maybe a) -> a -> Parser s m ()
writeState l a = modifyState (L.set l (return a))

readState :: Monad m => L.Lens' ParserState (Maybe a) -> Parser s m a
readState l = do
  st <- getState
  maybe (fail "required state missig") return (L.view l st)

tryOrState :: Monad m => Parser s m a -> L.Lens' ParserState (Maybe a) -> Parser s m a
tryOrState p l = tryP <|> readState l
  where
    tryP = do
      a <- try p
      writeState l a
      return a
