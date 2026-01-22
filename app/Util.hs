module Util
  ( sepBy
  , parseZoneFileZones ) where

import Data.List (uncons)
import GHC.Base (failIO)
import Text.Parsec (ParseError)

import NameConstraint (NameConstraint, compileWildcards)
import Parsing (parseName, parseZoneFile)
import ZoneFile (Name, Zone)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileZones :: FilePath -> IO (Name String, [Zone (NameConstraint String)])
parseZoneFileZones path = do
  content <- readFile path
  zones <- parseZoneFile path content >>= handleParser
  ns <- parseName path >>= handleParser
  return (ns, map compileWildcards zones)
