module Parsing.Wellformedness
  ( wellformed ) where

import Data.Functor (($>))
import Control.Applicative (asum)
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import ZoneFile

wellformed :: Zone String -> Maybe String
wellformed (Zone org r@(Node apexData _)) =
  asum  [ guard wildcardOrigin $> "zone origin has wildcard"
        , guard noSOA $> "zone misses SOA record"
        , guard (recordsBeyondCNAMEDelegation r) $> "aliased zone has records"
        , guard (wildcardRestrictions orgLabel r) $> "DNAME or NS wildcard record" ]
  where
    orgLabel :: String
    orgLabel = NE.head $ labels org

    wildcardOrigin :: Bool
    wildcardOrigin = "*" == orgLabel

    noSOA :: Bool
    noSOA = all (\rd -> rtype rd /= SOA) apexData

    recordsBeyondCNAMEDelegation :: Node String -> Bool
    recordsBeyondCNAMEDelegation (Node rs cs) =
      if   any (\rd -> rtype rd == CNAME) rs
      then length rs > 1 || not (M.null cs)
      else any recordsBeyondCNAMEDelegation (M.elems cs)

    wildcardRestrictions :: String -> Node String -> Bool
    wildcardRestrictions label (Node rs cs)
      | label == "*" && M.null cs = any (\rd -> rtype rd `elem` [DNAME, NS]) rs
      | otherwise = any (uncurry wildcardRestrictions) (M.toList cs)
