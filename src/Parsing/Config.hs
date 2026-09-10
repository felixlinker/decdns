{-# LANGUAGE TupleSections #-}
module Parsing.Config
  ( parseZoneFileZones
  , parseZoneFiles
  , parseStdin
  ) where

import qualified Data.List.NonEmpty as NE
import GHC.Base (failIO, when)
import Text.Parsec (ParseError, SourceName)

import DNS.Name (Name)
import DNS.ZoneFile (Zone, fromRecordList)
import DNS.Zone (splitZone)
import NameConstraint (compileWildcards)
import Parsing (ServedZone, parseZoneFile)
import Resolvable.Resolvability (NameServersConfig)
import DNS.Wellformedness (Issue, isFatal)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

parseZoneFileContents :: (Zone String -> [Issue String]) -> SourceName -> String -> IO NameServersConfig
parseZoneFileContents wfc source content = do
  parsed <- handleParser =<< parseZoneFile source content
  zones <- concat <$> mapM toZones parsed
  mapM_ (wellformed . snd) zones
  return $ map (fmap compileWildcards) zones
  where
    wellformed :: Zone String -> IO (Zone String)
    wellformed zone = do
      let issues = wfc zone
      mapM_ print issues
      when (any isFatal issues) (fail "fatal error encountered")
      return zone

    toZones :: ServedZone -> IO [(Name String, Zone String)]
    toZones (nameServer, records) = do
      zones <- maybe (fail "no records") (return . splitZone . fromRecordList) (NE.nonEmpty records)
      return $ map (nameServer,) zones

parseStdin :: (Zone String -> [Issue String]) -> IO NameServersConfig
parseStdin wfc = do
  content <- getContents
  parseZoneFileContents wfc "<stdin>" content

parseZoneFileZones :: (Zone String -> [Issue String]) -> FilePath -> IO NameServersConfig
parseZoneFileZones wfc path = do
  content <- readFile path
  parseZoneFileContents wfc path content

parseZoneFiles :: (Zone String -> [Issue String]) -> [FilePath] -> IO NameServersConfig
parseZoneFiles wfc = fmap concat . mapM (parseZoneFileZones wfc)
