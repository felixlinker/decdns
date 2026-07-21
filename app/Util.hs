{-# LANGUAGE ViewPatterns #-}
module Util
  ( sepBy
  , parseZoneFileZones
  , parseNames ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Data.List (uncons)
import GHC.Base (failIO)
import Text.Parsec (ParseError)

import NameConstraint (compileWildcards)
import Parsing (parseName, parseZoneFile, ServedZone)
import DNS.ZoneFile (Zone, fromRecordList)
import DNS.Name (Name)
import Parsing.Wellformedness (wellformed)
import Resolvable.Resolvability (NameServersConfig)

sepBy :: String -> [String] -> IO ([String], [String])
sepBy sep args = do
  let (first, withSep) = span (/= sep) args
  (_, second) <- maybe (failIO "missing separator") return (uncons withSep)
  return (first, second)

handleParser :: Either ParseError a -> IO a
handleParser = either (failIO . show) return

-- | Parse a zone file and return its origin together with compiled zones.
parseZoneFileZones :: FilePath -> IO NameServersConfig
parseZoneFileZones path = do
  content <- readFile path
  exceptZones <- map (runExcept . toZone) <$> (handleParser =<< parseZoneFile path content)
  zones <- mapM (either failIO return) exceptZones
  return $ map (fmap (map compileWildcards)) zones
  where
    wf :: Name String -> Zone String -> Except String ()
    wf ns zone = maybe (return ()) throwE (wellformed ns zone)

    toZone :: ServedZone -> Except String (Name String, [Zone String])
    toZone (ns, fromRecordList -> zs) = do
      mapM_ (wf ns) zs
      return (ns, zs)

parseNames :: [String] -> IO [Name String]
parseNames = mapM (handleParser <=< parseName)
