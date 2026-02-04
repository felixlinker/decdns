{-# LANGUAGE ViewPatterns #-}
module DNS.ZoneFile
  ( Node(..)
  , emptyZone
  , Zone(..)
  , insert
  , allRecords ) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Record

data Node a = Node
  { records :: [RData]
  , children :: M.Map a (Node a) }

emptyNode :: Node a
emptyNode = Node [] M.empty

data Zone a = Zone
  { zoneName :: Name a
  , root :: Node a }

emptyZone :: Name a -> Zone a
emptyZone org = Zone org emptyNode

pathInZone :: Eq a => Name a -> Name a -> Maybe [a]
pathInZone (Name (reverse . NE.toList -> org)) (Name (reverse . NE.toList -> n)) = pathInZone' org n
  where
    pathInZone' :: Eq a => [a] -> [a] -> Maybe [a]
    pathInZone' [] bs = Just bs
    pathInZone' _ [] = Just []
    pathInZone' (a:as) (b:bs)
      | a == b = pathInZone' as bs
      | otherwise = Nothing

insert :: Ord a => Zone a -> Record (Name a) -> Maybe (Zone a)
insert (Zone norg zroot) (Record n d) = do
  p <- pathInZone norg n
  return $ Zone norg (go p zroot)
  where
    go :: Ord a => [a] -> Node a -> Node a
    go [] node@(Node rs _) = node { records = d:rs }
    go (lr:t) node@(Node _ ch) =
      let chf = if M.member lr ch
            then M.update (Just . go t) lr
            else M.insert lr (go t emptyNode)
      in node { children = chf ch }

allRecords :: Zone a -> [Record (Name a)]
allRecords (Zone orig ns) = go orig ns
  where
    go :: Name a -> Node a -> [Record (Name a)]
    go n (Node rs ch) =
      map (Record n) rs ++ concat (M.mapWithKey (\l -> go (l <| n)) ch)
