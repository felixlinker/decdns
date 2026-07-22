module Util
  ( sepBy
  , parseZoneFileZones
  , parseStdin
  , parseNames ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Data.List (uncons)
import qualified Data.List.NonEmpty as NE
import GHC.Base (failIO)
import Text.Parsec (ParseError, SourceName)

import NameConstraint (compileWildcards)
import Parsing (parseName, parseZoneFile, ServedZone)
import DNS.ZoneFile (Zone, fromRecordList)
import DNS.Name (Name)
import Resolvable.Resolvability (NameServersConfig)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileContents :: (Zone String -> Maybe String) -> SourceName -> String -> IO NameServersConfig
parseZoneFileContents wfc s content = do
  exceptZones <- map (runExcept . toZone) <$> (handleParser =<< parseZoneFile s content)
  zones <- mapM (either failIO return) exceptZones
  return $ map (fmap compileWildcards) zones
  where
    wf :: Zone String -> Except String ()
    wf zone = maybe (return ()) throwE (wfc zone)

    toZone :: ServedZone -> Except String (Name String, Zone String)
    toZone (ns, rs) = do
      z <- maybe (throwE "no records") (return . fromRecordList) (NE.nonEmpty rs)
      wf z
      return (ns, z)

parseStdin :: (Zone String -> Maybe String) -> IO NameServersConfig
parseStdin wfc = do
  content <- getContents
  parseZoneFileContents wfc "<stdin>" content

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileZones :: (Zone String -> Maybe String) -> FilePath -> IO NameServersConfig
parseZoneFileZones wfc path = do
  content <- readFile path
  parseZoneFileContents wfc path content

parseNames :: [String] -> IO [Name String]
parseNames = mapM (handleParser <=< parseName)
