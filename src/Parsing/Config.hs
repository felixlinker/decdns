{-# LANGUAGE TupleSections #-}
module Parsing.Config
  ( parseZoneFileZones
  , parseZoneFiles
  , parseStdin
  ) where

import qualified Data.Set as S
import GHC.Base (failIO, when)
import System.IO (stderr, hPrint)
import Text.Parsec (ParseError, SourceName)

import DNS.ZoneFile (Zone)
import DNS.Zone (splitZones)
import NameConstraint (compileWildcards)
import Parsing (parseZoneFile)
import Resolvable.Resolvability (NameServersConfig)
import DNS.Wellformedness (Issue, isFatal)
import Util.Parsing

parseZoneFileContents :: (Zone String -> [Issue String]) -> SourceName -> String -> IO NameServersConfig
parseZoneFileContents wfc source content = do
  parsed <- handleParser =<< parseZoneFile source content
  let zones = splitZones (const S.empty) parsed
  mapM_ (wellformed . snd) zones
  return $ map (fmap compileWildcards) zones
  where
    wellformed :: Zone String -> IO (Zone String)
    wellformed zone = do
      let issues = wfc zone
      mapM_ (hPrint stderr) issues
      when (any isFatal issues) (fail "fatal error encountered")
      return zone

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
