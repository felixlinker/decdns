{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Util.PartialOrder
  ( PartialOrder
  , empty
  , Util.PartialOrder.null
  , elems
  , toList
  , toFlatList
  , toAscList
  , fromList
  , smaller
  , minima
  , minimal
  , transitiveClosure
  , irreflexive
  , invert
  , insert
  , remove
  , Util.PartialOrder.filter
  , tarjan ) where

import qualified Control.Monad.Trans.State as St
import Control.Monad (foldM)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (partition)
import Data.Bifunctor (Bifunctor(first))
import Data.Tuple (swap)

-- po maps a key to all elements that are smaller than the key
newtype PartialOrder a = PartialOrder { toSmaller :: M.Map a (S.Set a) }

empty :: PartialOrder a
empty = PartialOrder M.empty

null :: PartialOrder a -> Bool
null = M.null . toSmaller

elems :: Ord a => PartialOrder a -> S.Set a
elems (PartialOrder m) = foldr S.union (M.keysSet m) (M.elems m)

toList :: PartialOrder a -> [(S.Set a, a)]
toList = toAscList

toFlatList :: PartialOrder a -> [(a, a)]
toFlatList = concatMap (\(as, a) -> map (,a) (S.toList as)) . toList

toAscList :: PartialOrder a -> [(S.Set a, a)]
toAscList = map swap . M.toAscList . toSmaller

fromList :: Ord a => [(a, a)] -> PartialOrder a
fromList = foldr (uncurry insert . first S.singleton) empty

smaller :: Ord a => PartialOrder a -> a -> S.Set a
smaller po a = fromMaybe S.empty $ M.lookup a (toSmaller po)

minima :: Ord a => PartialOrder a -> S.Set a
minima = S.fromAscList . map fst . Prelude.filter (S.null . snd) . M.toAscList . toSmaller

minimal :: Ord a => PartialOrder a -> a -> Bool
minimal po = S.null . smaller po

transitiveClosure :: forall a. Ord a => PartialOrder a -> Maybe (PartialOrder a)
transitiveClosure (PartialOrder d) =
  let (mins, recArg) = M.partition S.null d
  in PartialOrder <$> rec (M.toList recArg) mins
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

irreflexive :: Eq a => PartialOrder a -> PartialOrder a
irreflexive (PartialOrder m) = PartialOrder $ M.mapWithKey (\k -> S.filter (/= k)) m

invert :: Ord a => PartialOrder a -> PartialOrder a
invert (PartialOrder m) = foldr (uncurry foldF) empty (M.toList m)
  where
    foldF :: Ord a => a -> S.Set a -> PartialOrder a -> PartialOrder a
    foldF oldK oldVs d = foldr (insert (S.singleton oldK)) d oldVs

insert :: Ord a => S.Set a -> a -> PartialOrder a -> PartialOrder a
insert vs k (PartialOrder m) = PartialOrder (M.alter (Just . maybe vs (vs <>)) k m)

remove :: Ord a => a -> PartialOrder a -> PartialOrder a
remove key (PartialOrder m) = PartialOrder $ M.map (S.delete key) $ M.delete key m

filter :: (a -> Bool) -> PartialOrder a -> PartialOrder a
filter p = PartialOrder . M.map (S.filter p) . M.filterWithKey (\k _ -> p k) . toSmaller

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
tarjan :: forall a. Ord a => PartialOrder a -> [S.Set a]
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
          (unexplored, explored) <- getsNodes (\ns -> S.partition (`M.notMember` ns) (smaller po v))
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
