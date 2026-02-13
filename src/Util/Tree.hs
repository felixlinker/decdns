module Util.Tree
  ( Node(..)
  , emptyNode
  , insert
  , mapItems
  , fmerge
  , merge ) where

import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM
import Data.Maybe (fromMaybe)

data Node k v = Node
  { items :: [v]
  , children :: M.Map k (Node k v) }

instance Functor (Node k) where
  fmap f (Node rs cs) = Node (fmap f rs) (M.map (fmap f) cs)

emptyNode :: Node a v
emptyNode = Node [] M.empty

insert :: Ord k => [k] -> v -> Node k v -> Node k v
insert [] v n@(Node ds _) = n { items = v:ds }
insert (h:t) v n@(Node _ cs) =
  let rec = insert t v $ fromMaybe emptyNode $ M.lookup h cs
  in  n { children = M.insert h rec cs }

mapItems :: ([a] -> [b]) -> Node k a -> Node k b
mapItems f (Node is cs) = Node (f is) (M.map (mapItems f) cs)

fmerge :: Ord k => ([v1] -> [v2] -> [v3]) -> Node k v1 -> Node k v2 -> Node k v3
fmerge f (Node i1 cs1) (Node i2 cs2) =
  Node (f i1 i2) (MM.merge (MM.mapMissing (const emptyL)) (MM.mapMissing (const emptyR)) (MM.zipWithMatched (const (fmerge f))) cs1 cs2)
  where
    emptyL = mapItems (`f` [])
    emptyR = mapItems (f [])

merge :: Ord k => Node k v -> Node k v -> Node k v
merge = fmerge (++)
