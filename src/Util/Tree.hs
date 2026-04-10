{-# LANGUAGE ScopedTypeVariables #-}
module Util.Tree
  ( Node(..)
  , Util.Tree.null
  , prepend
  , leaf
  , leafAt
  , emptyNode
  , prune
  , get
  , adjust
  , insertNode
  , insert
  , mapItems
  , fmerge
  , merge
  , partition ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM

import Util.Func

data Node k v = Node
  { items :: [v]
  , children :: M.Map k (Node k v) }
  deriving Eq

instance (Show k, Show v) => Show (Node k v) where
  show (Node is cs) = unlines
    ( ("Node " ++ show is ++ "; children:")
    : indent (concatMap (uncurry showChild) (M.toList cs)))
    where
      indent :: [String] -> [String]
      indent = map ("  " ++)

      showChild :: k -> Node k v -> [String]
      showChild k n = ["/" ++ show k, show n]

instance Foldable (Node k) where
  foldr f acc (Node vs cs) = foldr (flip (foldr f)) (foldr f acc vs) cs

instance Functor (Node k) where
  fmap f (Node rs cs) = Node (fmap f rs) (M.map (fmap f) cs)

null :: Node k v -> Bool
null (Node is cs) = Prelude.null is && M.null cs

prepend :: [a] -> Node a v -> Node a v
prepend path n = foldr (Node [] <.> M.singleton) n path

leaf :: [v] -> Node a v
leaf as = Node as M.empty

leafAt :: [a] -> [v] -> Node a v
leafAt p = prepend p . leaf

emptyNode :: Node a v
emptyNode = leaf []

prune :: Node a v -> Node a v
prune n@(Node _ cs) = n { children = M.filter (not . Util.Tree.null) (M.map prune cs) }

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

adjust :: forall k v. Ord k => ([k] -> Node k v -> Node k v) -> [k] -> Node k v -> Node k v
adjust nf = go
  where
    go :: [k] -> Node k v -> Node k v
    go [] n = nf [] n
    go p@(h:t) n@(Node _ cs) =
      if h `M.member` cs
        then n { children = M.adjust (go t) h cs }
        else nf p n

insertNode :: forall k v. Ord k => [k] -> Node k v -> Node k v -> Node k v
insertNode p new = adjust insF p
  where
    insF :: [k] -> Node k v -> Node k v
    insF [] _ = new
    insF (h:t) n@(Node _ cs) = n { children = M.insert h (prepend t new) cs }

insert :: forall k v. Ord k => [k] -> v -> Node k v -> Node k v
insert p v = adjust insF p
  where
    insF :: [k] -> Node k v -> Node k v
    insF [] n@(Node is _) = n { items = v:is }
    insF (h:t) n@(Node _ cs) = n { children = M.insert h (leafAt t [v]) cs }

partition :: forall k v. ([k] -> v -> Bool) -> Node k v -> (Node k v, Node k v)
partition p = go []
  where
    go :: [k] -> Node k v -> (Node k v, Node k v)
    go path (Node vs cs) =
      let (top, bot) = L.partition (p path) vs
          rec = M.mapWithKey (go . (:path)) cs
      in (Node top (M.map fst rec), Node bot (M.map snd rec))
