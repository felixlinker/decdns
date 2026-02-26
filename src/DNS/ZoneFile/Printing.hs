{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile.Printing
  ( printZone
  , printZones
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import qualified Control.Monad.Trans.State as St

import DNS.Name (Name(Name))
import DNS.Record (RData (RData))
import DNS.ZoneFile

type State = (String, Int)  -- (last name, last TTL)

showRecord :: Bool -> [String] -> RData -> St.State State String
showRecord abrev labels (RData (show -> c) t (show -> d)) = do
  (lastName, lastTTL) <- St.get
  let rname = maybe "@" (show . Name) (NE.nonEmpty labels)
  let thisName = fromLast lastName rname
  let thisTTL = fromLast lastTTL t
  St.put (fromMaybe lastName thisName, fromMaybe lastTTL thisTTL)
  return $ indent $ unwords (catMaybes [thisName, show <$> thisTTL, Just c, Just d])
  where
    indent :: String -> String
    indent = ("  " ++)

    fromLast :: Eq a => a -> a -> Maybe a
    fromLast l this
      | l == this && abrev = Nothing
      | otherwise = Just this

printZone :: Bool -> Zone String -> [String]
printZone abrev z@(Zone org _) = ("$ORIGIN " ++ show org) : zshow (showRecord abrev) ("", -1) z

printZones :: Bool -> [Zone String] -> [String]
printZones = concatMap . printZone
