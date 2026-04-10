module Parsing.Wellformedness
  ( wellformed ) where

import Data.Functor (($>),(<&>))
import Data.List (find)
import Control.Applicative (asum, (<|>))
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.ZoneFile
import DNS.Record
import qualified DNS.RecordType as T
import qualified Util.Tree as Tr

data ZoneCut = CNAMECut | NSCut (Name String)

getZoneCut :: Name String -> [RData] -> Maybe ZoneCut
getZoneCut n rs
  | any (isType T.CNAME) rs = Just CNAMECut
  | any (isType T.NS) rs = Just (NSCut n)
  | otherwise = Nothing

wellformed :: Zone String -> Maybe String
wellformed (Zone org r@(Tr.Node apexData rcs)) =
  asum  [ guard wildcardOrigin $> "zone origin has wildcard"
        , guard noSOA $> "zone misses SOA record"
        , guard (any (isType T.CNAME) apexData) $> "no CNAME at APEX"
        , checkZoneCuts org (Tr.Node (filter (not . isType T.NS) apexData) rcs) <&> ("illegal record at or below zone cut: " ++) . show
        , guard (wildcardRestrictions orgLabel r) $> "DNAME or NS wildcard record" ]
  where
    orgLabel :: String
    orgLabel = NE.head $ labels org

    wildcardOrigin :: Bool
    wildcardOrigin = "*" == orgLabel

    noSOA :: Bool
    noSOA = all (\rd -> toType (rtype rd) /= T.SOA) apexData

    checkZoneCuts :: Name String -> Tr.Node String RData -> Maybe StandardRecord
    checkZoneCuts n (Tr.Node rs cs) = maybe rec recCheck (getZoneCut n rs)
      where
        rec :: Maybe StandardRecord
        rec = asum $ M.mapWithKey (\k -> checkZoneCuts (k <| n)) cs

        recCheck :: ZoneCut -> Maybe StandardRecord
        recCheck cut = find (not . isAllowedAtZoneCut cut) (map (Named n) rs)
            <|> find (not . isAllowedBelowZoneCut cut) (allData (Zone n (Tr.Node [] cs)))

isType :: T.Type -> RData -> Bool
isType t = (t ==) . toType . rtype

isAllowedAtZoneCut :: ZoneCut -> StandardRecord -> Bool
isAllowedAtZoneCut cut (Named _ rd) =
     isCutRecord cut rd
  || any ($ rd) [isType T.DS, isType T.NSEC, isType T.RRSIG]

isCutRecord :: ZoneCut -> RData -> Bool
isCutRecord CNAMECut = isType T.CNAME
isCutRecord (NSCut _) = isType T.NS

isAllowedBelowZoneCut :: ZoneCut -> StandardRecord -> Bool
isAllowedBelowZoneCut CNAMECut _ = False
isAllowedBelowZoneCut (NSCut delegN) (Named n rd) =
  n `isSubDomain` delegN && any ($ rd) [isType T.A, isType T.AAAA]

wildcardRestrictions :: String -> Tr.Node String RData -> Bool
wildcardRestrictions label (Tr.Node rs cs)
  | label == "*" && M.null cs = any (\rd -> toType (rtype rd) `elem` [T.DNAME, T.NS]) rs
  | otherwise = any (uncurry wildcardRestrictions) (M.toList cs)
