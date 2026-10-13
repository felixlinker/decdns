module Util
  ( sepBy
  , parseZoneFileZones
  , parseStdin
  , parseNames
  , parseDS ) where

import Control.Monad ((<=<))
import Data.List (uncons)
import GHC.Base (failIO)

import Parsing (parseName)
import Parsing.Config (parseStdin, parseZoneFileZones, parseDS)
import DNS.Name (Name)
import Util.Parsing (handleParser)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

parseNames :: [String] -> IO [Name String]
parseNames = mapM (handleParser <=< parseName)
