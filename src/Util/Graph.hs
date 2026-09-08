{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Util.Graph
  ( Graph
  , empty
  , Util.Graph.null
  , elems
  , toList
  , toFlatList
  , toAscList
  , fromList
  , next
  , leaves
  , leaf
  , transitiveClosure
  , irreflexive
  , invert
  , insert
  , remove
  , Util.Graph.map
  , Util.Graph.filter
  , tarjan ) where

import qualified Control.Monad.Trans.State as St
import Control.Monad (foldM)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (partition)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe, fromJust)

-- edges maps a key to all nodes that have an edge from the key to one of the
-- values. This datastructure maintains the invariant that all values that occur
-- in any of edges values also occur as a key in edges.
newtype Graph a = Graph { edges :: M.Map a (S.Set a) }

instance Foldable Graph where
  foldr f acc = foldr f acc . M.keys . edges

empty :: Graph a
empty = Graph M.empty

null :: Graph a -> Bool
null = M.null . edges

elems :: Graph a -> S.Set a
elems = M.keysSet . edges

toList :: Graph a -> [(a, S.Set a)]
toList = toAscList

toFlatList :: Graph a -> [(a, a)]
toFlatList = concatMap (\(a, as) -> Prelude.map (a,) (S.toList as)) . toList

toAscList :: Graph a -> [(a, S.Set a)]
toAscList = M.toAscList . edges

fromList :: Ord a => [(a, a)] -> Graph a
fromList = closeLeaves . foldr (\(k, v) -> insertUnsafe k (S.singleton v)) empty

next :: Ord a => Graph a -> a -> Maybe (S.Set a)
next po a = M.lookup a (edges po)

leaves :: Ord a => Graph a -> S.Set a
leaves = S.fromAscList . Prelude.map fst . Prelude.filter (S.null . snd) . M.toAscList . edges

leaf :: Ord a => Graph a -> a -> Bool
leaf po = maybe False S.null . next po

transitiveClosure :: forall a. Ord a => Graph a -> Maybe (Graph a)
transitiveClosure (Graph d) =
  let (mins, recArg) = M.partition S.null d
  in Graph <$> rec (M.toList recArg) mins
  where
    rec :: [(a, S.Set a)] -> M.Map a (S.Set a) -> Maybe (M.Map a (S.Set a))
    rec [] acc = Just acc
    rec ls acc =
      let (ready, recArg) = partition (all closed . snd) ls
      in if Prelude.null ready
        then Nothing
        else rec recArg (foldr foldf acc ready)
      where
        closed :: a -> Bool
        closed a = M.member a acc || not (M.member a d)

    foldf :: (a, S.Set a) -> M.Map a (S.Set a) -> M.Map a (S.Set a)
    foldf (k, vs) acc =
      let vs' = foldr (\v s -> maybe s (<> s) (M.lookup v acc)) vs vs
      in M.insert k vs' acc

irreflexive :: Eq a => Graph a -> Graph a
irreflexive (Graph m) = Graph $ M.mapWithKey (\k -> S.filter (/= k)) m

invert :: Ord a => Graph a -> Graph a
invert = fromList . Prelude.map swap . toFlatList

-- NOTE: Must eventually call closeLeaves to maintain Graph.edges invariant
insertUnsafe :: Ord a => a -> S.Set a -> Graph a -> Graph a
insertUnsafe k vs = Graph . M.alter (Just . maybe vs (vs <>)) k . edges

closeLeaves :: Ord a => Graph a -> Graph a
closeLeaves (Graph m) = Graph $ foldr (flip (foldr (M.alter initEmpty))) m (M.elems m)
  where
    initEmpty :: Maybe (S.Set a) -> Maybe (S.Set a)
    initEmpty = Just . fromMaybe S.empty

insert :: Ord a => a -> S.Set a -> Graph a -> Graph a
insert k vs = closeLeaves . insertUnsafe k vs

remove :: Ord a => a -> Graph a -> Graph a
remove key (Graph m) = Graph $ M.map (S.delete key) $ M.delete key m

map :: forall a b. (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
map f (Graph m) = Graph $ foldr foldF M.empty (M.keys m)
  where
    foldF :: a -> M.Map b (S.Set b) -> M.Map b (S.Set b)
    foldF k =
      let img' = maybe S.empty (S.map f) (M.lookup k m)
      in  M.alter (Just . maybe img' (<> img')) (f k)

filter :: (a -> Bool) -> Graph a -> Graph a
filter p = Graph . M.map (S.filter p) . M.filterWithKey (\k _ -> p k) . edges

type Stack = []
type Index = Int
type LowLink = Int
type NodesMap a = M.Map a (Index, LowLink)

index :: (Index, LowLink) -> Index
index = fst

lowLink :: (Index, LowLink) -> LowLink
lowLink = snd

type TarjanSt a = St.State (Stack a, NodesMap a)

getsWith :: (s -> b) -> (b -> c) -> St.State s c
getsWith f g = St.gets (g . f)

getsNodes :: (NodesMap a -> v) -> St.State (x, NodesMap a) v
getsNodes = getsWith snd

modifyF :: Functor f => (b -> b) -> St.State (f b) ()
modifyF = St.modify . fmap

-- Following: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
tarjan :: forall a. Ord a => Graph a -> [S.Set a]
tarjan po = concat $ St.evalState (mapM go (S.toList $ elems po)) ([], M.empty)
  where
    go :: a -> TarjanSt a [S.Set a]
    go v = do
      mapped <- St.gets (M.member v . snd)
      if mapped
        then return []
        else do
          -- Insert v into state
          St.modify (\(stack, ns) -> (v:stack, M.insert v (M.size ns, M.size ns) ns))
          -- Prepare recursion
          (unexplored, explored) <- getsNodes (\ns -> S.partition (`M.notMember` ns) (fromJust $ next po v))
          -- Update lowLink for already explored nodes
          St.modify (\(stack, ns) ->
            let update = S.filter (`elem` stack) explored
            in (stack, foldr (\w -> updateLowLinkWith v w index) ns update))
          -- Recurse
          sccs <- foldM (\ses w -> do
            rec <- go w
            modifyF (updateLowLinkWith v w lowLink)
            return $ ses ++ rec
            ) [] unexplored
          -- SCC complete?
          isComplete <- getsNodes (maybe False (uncurry (==)) . M.lookup v)
          newSCC <- if isComplete then popStack v else return []
          return (newSCC ++ sccs)

    popStack :: a -> TarjanSt a [S.Set a]
    popStack v = do
      (stack, _) <- St.get
      case span (/= v) stack of
        (scc, v':rest) | v == v' -> do
          St.modify (\(_, ns) -> (rest, ns))
          return [S.fromList (v':scc)]
        _ -> error "v was not on stack"

    minMaybe :: Ord x => Maybe x -> x -> x
    minMaybe Nothing x = x
    minMaybe (Just x) y = min x y

    updateLowLinkWith :: a -> a -> ((Index, LowLink) -> Int) -> M.Map a (Index, LowLink) -> M.Map a (Index, LowLink)
    updateLowLinkWith v w with acc =
      let wVal = with <$> M.lookup w acc
      in  M.update (return . fmap (minMaybe wVal)) v acc
