module Util.Tree
  ( Node(..)
  , emptyNode
  , insert
  , mapItems
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

merge :: Ord k => Node k v -> Node k v -> Node k v
merge (Node i1 cs1) (Node i2 cs2) =
  Node (i1 ++ i2) (MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched (const merge)) cs1 cs2)
