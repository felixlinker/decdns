{-# LANGUAGE FlexibleContexts #-}
module Parsing.Dig
  ( received
  , served ) where

import Control.Monad (void)
import Text.Parsec

import DNS.Name ( Name )
import Parsing.Tokens
import Parsing.Name

received :: Stream s m Char => ParsecT s u m (Name String)
received = do
  spaces
  _ <- string ";; SERVER: "
  try (void ipv6) <|> void ipv4
  _ <- char '#'
  _ <- many digit
  _ <- char '('
  n <- absName
  _ <- string ") (UDP)"
  return n

served :: Stream s m Char => ParsecT s u m (Name String)
served = do
  spaces
  _ <- string ";; Served by "
  absName
