module Util.Tree
  ( Node(..)
  , leaf
  , emptyNode
  , get
  , alter
  , insertNode
  , insert
  , mapItems
  , fmerge
  , merge ) where

import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

data Node k v = Node
  { items :: [v]
  , children :: M.Map k (Node k v) }

instance Functor (Node k) where
  fmap f (Node rs cs) = Node (fmap f rs) (M.map (fmap f) cs)

leaf :: [v] -> Node a v
leaf as = Node as M.empty

emptyNode :: Node a v
emptyNode = leaf []

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

get :: Ord k => [k] -> Node k v -> Maybe [v]
get [] (Node is _) = Just is
get (h:t) (Node _ cs) = get t =<< M.lookup h cs

alter :: Ord k => (Node k v -> Node k v) -> Maybe (Node k v) -> [k] -> Node k v -> Node k v
alter nf _ [] n = nf n
alter nf def (h:t) (Node is cs) =
  let rec = alter nf def t
  in Node is $ M.alter (maybe (rec <$> def) (Just . rec)) h cs

insertNode :: Ord k => [k] -> Node k v -> Node k v -> Node k v
insertNode p new = alter (const new) (Just emptyNode) p

insert :: Ord k => [k] -> v -> Node k v -> Node k v
insert p v = alter (\n@(Node is _) -> n { items = v:is }) (Just emptyNode) p
