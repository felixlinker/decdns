{-# LANGUAGE FlexibleContexts #-}
module Parsing.Dig
  ( received
  , served ) where

import Control.Monad (void)
import Text.Parsec

import DNS.Name ( Name, (|>) )
import Parsing.Tokens
import Parsing.Name

received :: Stream s m Char => ParsecT s u m (Name String)
received = do
  spaces
  _ <- string ";; Received "
  _ <- many digit
  _ <- string " bytes from "
  try (void ipv4) <|> void ipv6
  _ <- char '#'
  _ <- many digit
  _ <- char '('
  n <- relName
  _ <- string ") in "
  _ <- many1 digit
  _ <- string " ms"
  -- Make absolute because names in dig are expressed relatively
  return $ n |> ""

served :: Stream s m Char => ParsecT s u m (Name String)
served = do
  spaces
  _ <- string ";; Served by "
  absName
