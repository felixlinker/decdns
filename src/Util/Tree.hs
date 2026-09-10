{-# LANGUAGE ScopedTypeVariables #-}
module Util.Tree
  ( Node(..)
  , Edges
  , Util.Tree.null
  , prepend
  , leaf
  , leafAt
  , emptyNode
  , prune
  , get
  , adjustF
  , adjust
  , insertNode
  , insert
  , mapItems
  , fmerge
  , merge
  , partition
  , mapLabels
  , mapNamed
  , traverseNamed
  , allNamed ) where

import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MM
import DNS.Name ( (<|), Name )

import Util.Func
import Util.Map (alterDefF)

type Edges k v = M.Map k (Node k v)
data Node k v = Node
  { item :: v
  , children :: Edges k v }
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
  foldr f acc (Node vs cs) = foldr (flip (foldr f)) (f vs acc) cs

instance Functor (Node k) where
  fmap f (Node i cs) = Node (f i) (M.map (fmap f) cs)

null :: Foldable t => Node k (t a) -> Bool
null (Node is cs) = Prelude.null is && M.null cs

prepend :: Monoid v => [a] -> Node a v -> Node a v
prepend path n = foldr (Node mempty <.> M.singleton) n path

leaf :: v -> Node a v
leaf i = Node i M.empty

leafAt :: Monoid v => [a] -> v -> Node a v
leafAt p = prepend p . leaf

emptyNode :: Monoid v => Node a v
emptyNode = leaf mempty

prune :: Foldable t => Node a (t v) -> Node a (t v)
prune n@(Node _ cs) = n { children = M.filter (not . Util.Tree.null) (M.map prune cs) }

mapItems :: (a -> b) -> Node k a -> Node k b
mapItems f (Node is cs) = Node (f is) (M.map (mapItems f) cs)

fmerge :: (Monoid v1, Monoid v2, Ord k) => (v1 -> v2 -> v3) -> Node k v1 -> Node k v2 -> Node k v3
fmerge f (Node i1 cs1) (Node i2 cs2) =
  Node (f i1 i2) (MM.merge (MM.mapMissing (const emptyL)) (MM.mapMissing (const emptyR)) (MM.zipWithMatched (const (fmerge f))) cs1 cs2)
  where
    emptyL = mapItems (`f` mempty)
    emptyR = mapItems (f mempty)

merge :: (Monoid v, Ord k) => Node k v -> Node k v -> Node k v
merge = fmerge (<>)

get :: Ord k => [k] -> Node k v -> Maybe v
get [] (Node i _) = Just i
get (h:t) (Node _ cs) = get t =<< M.lookup h cs

adjustF :: forall f k v. (Applicative f, Ord k) => ([k] -> Node k v -> f (Node k v)) -> [k] -> Node k v -> f (Node k v)
adjustF nf = go
  where
    go :: [k] -> Node k v -> f (Node k v)
    go [] n = nf [] n
    go p@(h:t) n@(Node v cs) =
      if h `M.member` cs
        then Node v <$> alterDefF (go t) h cs
        else nf p n

adjust :: forall k v. Ord k => ([k] -> Node k v -> Node k v) -> [k] -> Node k v -> Node k v
adjust f p = runIdentity . adjustF (\p' -> return . f p') p

insertNode :: forall k v. (Monoid v, Ord k) => [k] -> Node k v -> Node k v -> Node k v
insertNode p new = adjust insF p
  where
    insF :: [k] -> Node k v -> Node k v
    insF [] _ = new
    insF (h:t) n@(Node _ cs) = n { children = M.insert h (prepend t new) cs }

insert :: forall k v. (Monoid v, Ord k) => [k] -> v -> Node k v -> Node k v
insert p v = adjust insF p
  where
    insF :: [k] -> Node k v -> Node k v
    insF [] n@(Node is _) = n { item = is <> v }
    insF (h:t) n@(Node _ cs) = n { children = M.insert h (leafAt t v) cs }

partition :: forall k v. ([k] -> v -> (v, v)) -> Node k v -> (Node k v, Node k v)
partition p = go []
  where
    go :: [k] -> Node k v -> (Node k v, Node k v)
    go path (Node v cs) =
      let (top, bot) = p path v
          rec = M.mapWithKey (go . (:path)) cs
      in (Node top (M.map fst rec), Node bot (M.map snd rec))

mapLabels :: (Ord a, Ord b) => (a -> b) -> Node a v -> Node b v
mapLabels f (Node i cs) = Node i $ M.mapKeys f (M.map (mapLabels f) cs)

mapNamed :: (Name a -> u -> v) -> Name a -> Node a u -> Node a v
mapNamed f owner (Node d cs) = Node (f owner d) (M.mapWithKey (\k -> mapNamed f (k <| owner)) cs)

traverseNamed :: Applicative f => (Name a -> u -> f v) -> Name a -> Node a u -> f (Node a v)
traverseNamed f owner (Node d cs) = Node <$> f owner d <*> M.traverseWithKey (\k -> traverseNamed f (k <| owner)) cs

allNamed :: (Name a -> v -> Bool) -> Name a -> Node a v -> Bool
allNamed f owner n = and (mapNamed f owner n)
