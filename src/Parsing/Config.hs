module Parsing.Config
  ( parseZoneFileContents
  , parseZoneFileZones
  , parseStdin
  ) where

import Control.Monad.Trans.Except (Except, runExcept, throwE)
import qualified Data.List.NonEmpty as NE
import GHC.Base (failIO)
import Text.Parsec (ParseError, SourceName)

import DNS.Name (Name)
import DNS.ZoneFile (Zone, fromRecordList)
import NameConstraint (compileWildcards)
import Parsing (ServedZone, parseZoneFile)
import Resolvable.Resolvability (NameServersConfig)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

parseZoneFileContents :: (Zone String -> Maybe String) -> SourceName -> String -> IO NameServersConfig
parseZoneFileContents wfc source content = do
  exceptZones <- map (runExcept . toZone) <$> (handleParser =<< parseZoneFile source content)
  zones <- mapM (either failIO return) exceptZones
  return $ map (fmap compileWildcards) zones
  where
    wellformed :: Zone String -> Except String ()
    wellformed zone = maybe (return ()) throwE (wfc zone)

    toZone :: ServedZone -> Except String (Name String, Zone String)
    toZone (nameServer, records) = do
      zone <- maybe (throwE "no records") (return . fromRecordList) (NE.nonEmpty records)
      wellformed zone
      return (nameServer, zone)

parseStdin :: (Zone String -> Maybe String) -> IO NameServersConfig
parseStdin wfc = do
  content <- getContents
  parseZoneFileContents wfc "<stdin>" content

parseZoneFileZones :: (Zone String -> Maybe String) -> FilePath -> IO NameServersConfig
parseZoneFileZones wfc path = do
  content <- readFile path
  parseZoneFileContents wfc path content
