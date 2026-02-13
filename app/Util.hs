module Util
  ( sepBy
  , parseZoneFileZones
  , parseNames ) where

import Control.Monad ((<=<))
import Data.List (uncons)
import GHC.Base (failIO)
import Text.Parsec (ParseError)

import NameConstraint (ConstrainedRecord, compileWildcards)
import Parsing (parseName, parseZoneFile)
import DNS.ZoneFile (allData)
import DNS.Name (Name)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileZones :: FilePath -> IO (Name String, [ConstrainedRecord])
parseZoneFileZones path = do
  content <- readFile path
  zones <- parseZoneFile path content >>= handleParser
  ns <- parseName path >>= handleParser
  return (ns, concatMap (allData . compileWildcards) zones)

parseNames :: [String] -> IO [Name String]
parseNames = mapM (handleParser <=< parseName)
