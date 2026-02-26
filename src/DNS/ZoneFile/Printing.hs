{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile.Printing
  ( printZone
  , printZones
  ) where

import Data.List (mapAccumL)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import DNS.Name (Name(Name))
import DNS.Record (RData (RData))
import DNS.ZoneFile
import qualified Util.Tree as T

showRecord :: Bool -> String -> String -> Int -> RData -> (Int, String)
showRecord abrev rname lastName lastTTL (RData (show -> c) t (show -> d)) =
  let thisName = fromLast lastName rname
      thisTTL = show <$> fromLast lastTTL t
  in (t, unwords (catMaybes [thisName, thisTTL, Just c, Just d]))
  where
    fromLast :: Eq a => a -> a -> Maybe a
    fromLast l this
      | l == this && abrev = Nothing
      | otherwise = Just this

printZone :: Bool -> Zone String -> [String]
printZone abrev (Zone rootn r) =
  -- We pick safe defaults for go, i.e., a name and TTL that are impossible
  ("$ORIGIN " ++ show rootn) : map indent (snd (go [] "" (-1) r))
  where
    indent :: String -> String
    indent = ("  " ++)

    go  :: [String] -- Current name (as list; empty on root node)
        -> String   -- Previous name
        -> Int      -- Previous ttl
        -> T.Node String RData
        -> (Int, [String])
    go nameAcc prevName prevTTL (T.Node d cs) =
      let (lastTTL, thisData) = mapAccumL (showRecord abrev showNameRel prevName) prevTTL d
          (lastTTL', recData) = mapAccumL (\ttlAcc (l, dat) -> go (l:nameAcc) showNameRel ttlAcc dat) lastTTL (M.toAscList cs)
       in  (lastTTL', thisData ++ concat recData)
       where
        showNameRel :: String
        showNameRel = maybe "@" (show . Name) (NE.nonEmpty nameAcc)

printZones :: Bool -> [Zone String] -> [String]
printZones = concatMap . printZone
