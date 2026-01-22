module Util
  ( parseZoneFileZones ) where

import GHC.Base (failIO)
import Text.Parsec (ParseError)

import NameConstraint (NameConstraint, compileWildcards)
import Parsing (parseName, parseZoneFile)
import ZoneFile (Name, Zone)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileZones :: FilePath -> IO (Name String, [Zone (NameConstraint String)])
parseZoneFileZones path = do
  content <- readFile path
  zones <- parseZoneFile path content >>= handleParser
  ns <- parseName path >>= handleParser
  return (ns, map compileWildcards zones)
