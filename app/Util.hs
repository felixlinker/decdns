module Util
  ( sepBy
  , parseZoneFileZones
  , parseStdin
  , parseNames ) where

import Control.Monad ((<=<))
import Data.List (uncons)
import GHC.Base (failIO)
import Text.Parsec (ParseError)

import Parsing (parseName)
import Parsing.Config (parseStdin, parseZoneFileZones)
import DNS.Name (Name)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

parseNames :: [String] -> IO [Name String]
parseNames = mapM (handleParser <=< parseName)
