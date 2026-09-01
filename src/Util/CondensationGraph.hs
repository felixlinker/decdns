{-# LANGUAGE ScopedTypeVariables #-}
module Util.CondensationGraph
  ( fromGraph ) where

import qualified Data.Set as S

import qualified Util.Graph as G

type CondensationGraph a = G.Graph (S.Set a)

fromGraph :: forall a. Ord a => G.Graph a -> CondensationGraph a
fromGraph g = foldr (\scc -> G.insert scc (S.delete scc $ S.fromList $ edges scc)) G.empty sccs
  where
    sccs :: [S.Set a]
    sccs = G.tarjan g

    edges :: S.Set a -> [S.Set a]
    edges node = filter (any (\(f, t) -> t `S.member` G.next g f) . S.cartesianProduct node) sccs
