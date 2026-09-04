{-# LANGUAGE TupleSections #-}
module Parsing.Config
  ( parseZoneFileZones
  , parseZoneFiles
  , parseStdin
  ) where

import Control.Monad.Trans.Except (Except, runExcept, throwE)
import qualified Data.List.NonEmpty as NE
import GHC.Base (failIO)
import Text.Parsec (ParseError, SourceName)

import DNS.Name (Name)
import DNS.ZoneFile (Zone, fromRecordList)
import DNS.Zone (splitZone)
import NameConstraint (compileWildcards)
import Parsing (ServedZone, parseZoneFile)
import Resolvable.Resolvability (NameServersConfig)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

parseZoneFileContents :: (Zone String -> Maybe String) -> SourceName -> String -> IO NameServersConfig
parseZoneFileContents wfc source content = do
  exceptZones <- map (runExcept . toZones) <$> (handleParser =<< parseZoneFile source content)
  zones <- concat <$> mapM (either failIO return) exceptZones
  return $ map (fmap compileWildcards) zones
  where
    wellformed :: Zone String -> Except String ()
    wellformed zone = maybe (return ()) throwE (wfc zone)

    toZones :: ServedZone -> Except String [(Name String, Zone String)]
    toZones (nameServer, records) = do
      zones <- maybe (throwE "no records") (return . splitZone . fromRecordList) (NE.nonEmpty records)
      mapM_ wellformed zones
      return $ map (nameServer,) zones

parseStdin :: (Zone String -> Maybe String) -> IO NameServersConfig
parseStdin wfc = do
  content <- getContents
  parseZoneFileContents wfc "<stdin>" content

parseZoneFileZones :: (Zone String -> Maybe String) -> FilePath -> IO NameServersConfig
parseZoneFileZones wfc path = do
  content <- readFile path
  parseZoneFileContents wfc path content

parseZoneFiles :: (Zone String -> Maybe String) -> [FilePath] -> IO NameServersConfig
parseZoneFiles wfc = fmap concat . mapM (parseZoneFileZones wfc)
