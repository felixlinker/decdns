{-# LANGUAGE FlexibleContexts #-}
module Parsing.Command
  ( Query
  , Command(..)
  , parseCommand ) where

import Control.Applicative (asum)
import Data.Functor (($>))
import Text.Parsec

import DNS.RecordType
import DNS.Name (Name)
import RecursiveResolver
import Parsing.Name
import Parsing.Data

type Query = (Name String, Type)

data Command = Exit | Query Query | SList SList | Print
  deriving Show

query :: Stream s m Char => ParsecT s () m Command
query = do
  _ <- string ":?"
  spaces
  n <- name
  spaces
  t <- recordType
  return $ Query (n, t)

nslist :: Stream s m Char => ParsecT s () m Command
nslist = do
  _ <- string ":slist"
  SList <$> many (spaces >> name)

exit :: Stream s m Char => ParsecT s u m Command
exit = string ":q" $> Exit

cprint :: Stream s m Char => ParsecT s u m Command
cprint = string ":print" $> Print

commands :: Stream s m Char => ParsecT s () m Command
commands = asum $ map try [ query, nslist, exit, cprint ]

parseCommand :: Monad m => String -> m (Either ParseError Command)
parseCommand = runParserT commands () "parseCommand function"
