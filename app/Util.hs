{-# LANGUAGE ViewPatterns #-}
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
import DNS.Wellformedness (wellformed)
import Resolvable.Resolvability (NameServersConfig)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileContents :: SourceName -> String -> IO NameServersConfig
parseZoneFileContents s content = do
  exceptZones <- map (runExcept . toZone) <$> (handleParser =<< parseZoneFile s content)
  zones <- mapM (either failIO return) exceptZones
  return $ map (fmap compileWildcards) zones
  where
    wf :: Zone String -> Except String ()
    wf zone = maybe (return ()) throwE (wellformed zone)

    toZone :: ServedZone -> Except String (Name String, Zone String)
    toZone (ns, rs) = do
      z <- maybe (throwE "no records") (return . fromRecordList) (NE.nonEmpty rs)
      wf z
      return (ns, z)

parseStdin :: IO NameServersConfig
parseStdin = do
  content <- getContents
  parseZoneFileContents "<stdin>" content

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileZones :: FilePath -> IO NameServersConfig
parseZoneFileZones path = do
  content <- readFile path
  parseZoneFileContents path content

parseNames :: [String] -> IO [Name String]
parseNames = mapM (handleParser <=< parseName)
