module Parsing.Wellformedness
  ( wellformed ) where

import Data.Functor (($>))
import Data.List (partition, find)
import Data.Maybe (mapMaybe)
import Control.Applicative (asum, (<|>))
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import ZoneFile
import RecursiveResolver (nameInRData)

wellformed :: Zone String -> Maybe String
wellformed (Zone org r@(Node apexData rcs)) =
  asum  [ guard wildcardOrigin $> "zone origin has wildcard"
        , guard noSOA $> "zone misses SOA record"
        , guard (recordsBeyondCNAMEDelegation r) $> "aliased zone has records"
        ,     ("non-glue record below zone cut: " ++) . show
              -- Filter NS in case this zone is authoritative over zone
          <$> recordsAfterZoneCut org (Node (filter ((NS /=) . rtype) apexData) rcs)
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

    recordsAfterZoneCut :: Name String -> Node String -> Maybe (Record (Name String))
    recordsAfterZoneCut n (Node rs cs)
      | null nsData = asum $ map (uncurry (recordsAfterZoneCut . (<| n))) (M.toList cs)
      -- Modify node to only include other records because names can self-delegate
      | otherwise = findNonGlue n (Node otherData cs)
      where
        nsData :: [RData]  -- NS records at this node
        otherData :: [RData] -- non-NS records at this node
        (nsData, otherData) = partition (\rd -> rtype rd == NS) rs

        nsesInBailiwick :: [Name String]
        nsesInBailiwick = filter (`isSubDomain` n) $ mapMaybe nameInRData nsData

        findNonGlue :: Name String -> Node String -> Maybe (Record (Name String))
        findNonGlue n' (Node rds cs') =
              Record n' <$> find (not . isGlue n') rds
          <|> asum (M.mapWithKey (findNonGlue . (<| n')) cs')

        isGlue :: Name String -> RData -> Bool
        isGlue rn rd = rn `elem` nsesInBailiwick && (rtype rd == A || rtype rd == AAAA)

    wildcardRestrictions :: String -> Node String -> Bool
    wildcardRestrictions label (Node rs cs)
      | label == "*" && M.null cs = any (\rd -> rtype rd `elem` [DNAME, NS]) rs
      | otherwise = any (uncurry wildcardRestrictions) (M.toList cs)
