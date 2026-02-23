{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile.Printing
  ( printZone
  , printZones
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import DNS.Record (RData (RData))
import DNS.ZoneFile
import qualified Util.Tree as T

showRecord :: String -> Int -> String -> RData -> (Int, String)
showRecord lastName lastTTL rname (RData (show -> c) t (show -> d)) =
  let thisName = fromLast lastName rname
      thisTTL = show <$> fromLast lastTTL t
  in (t, unwords (catMaybes [thisName, thisTTL, Just c, Just d]))
  where
    fromLast :: Eq a => a -> a -> Maybe a
    fromLast l this
      | l == this = Nothing
      | otherwise = Just this

printZone :: Zone String -> String
printZone (Zone rootn r) =
  -- We pick safe defaults for go, i.e., a name and TTL that are impossible
  unlines $ ("$ORIGIN " ++ show rootn) : go "" (-1) [] r
  where
    go  :: String   -- Previous name
        -> Int      -- Previous ttl
        -> [String] -- Current name (as list; empty on root node)
        -> T.Node String RData
        -> [String]
    go prevName prevTTL nameAcc (T.Node d cs) =
      let (lastTTL, printD) = foldl (uncurry foldf) (prevTTL, []) d
       in printD ++ concat (M.mapWithKey (\l -> go showNameRel lastTTL (l:nameAcc)) cs)
       where
        showNameRel :: String
        showNameRel = maybe "@" show (NE.nonEmpty nameAcc)
        foldf :: Int      -- Last TTL
              -> [String] -- Data accumulator
              -> RData    -- Data to print
              -> (Int, [String])
        foldf ttlAcc dataAcc dat = (:dataAcc) <$> showRecord prevName ttlAcc showNameRel dat

printZones :: [Zone String] -> String
printZones = unlines . map printZone
