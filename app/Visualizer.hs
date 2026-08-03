{-# LANGUAGE LambdaCase #-}
module Visualizer (renderVisualization) where

import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Numeric (showHex)

import DNS.Name (Name, Named(Named))
import DNS.Record (RData(..), toType)
import DNS.ZoneFile (allRRSets)
import NameConstraint (toStandardRecord)
import Resolvable.Dependencies
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Resolvability (NameServersConfig, RComp(..))
import Resolvable.Unification (toList)

renderVisualization :: [Name String] -> [FilePath] -> NameServersConfig -> S.Set (Rule (Maybe RComp)) -> String
renderVisualization roots zonePaths config rules = object
  [ field "version" "2"
  , field "rootServers" $ array $ map (string . show) roots
  , field "zoneFiles" $ array $ map string zonePaths
  , field "sources" $ array $ map renderSource sources
  , field "rules" $ array $ map renderRule ruleList
  , field "matches" $ array $ map renderMatch matchList
  ]
  where
    sources :: [RComp]
    sources = S.toList $ S.fromList
      [ RComp ns rrset
      | (ns, zone) <- config
      , rrset <- allRRSets zone
      ]

    ruleList :: [Rule (Maybe RComp)]
    ruleList = S.toList rules

    sourceIds :: M.Map RComp String
    sourceIds = M.fromList $ zip sources (map (("source-" ++) . show) [(1 :: Int)..])

    ruleIds :: M.Map (Rule (Maybe RComp)) String
    ruleIds = M.fromList $ zip ruleList (map (("rule-" ++) . show) [(1 :: Int)..])

    matchList :: [DependencyMatch]
    matchList = dependencyMatches rules

    renderSource :: RComp -> String
    renderSource source@(RComp ns (Named owner records)) = object
      [ field "id" $ string $ sourceIds M.! source
      , field "server" $ string $ show ns
      , field "owner" $ string $ show owner
      , field "recordType" $ string $ show $ toType $ rtype $ NE.head records
      , field "records" $ array $ map (string . show . toStandardRecord . Named owner) $ NE.toList records
      , field "ruleIds" $ array $ map string $ rulesFor source
      ]

    rulesFor :: RComp -> [String]
    rulesFor source = mapMaybe (ruleFor source) ruleList

    ruleFor :: RComp -> Rule (Maybe RComp) -> Maybe String
    ruleFor source rule
      | rannotation rule == Just source = M.lookup rule ruleIds
      | otherwise = Nothing

    renderRule :: Rule (Maybe RComp) -> String
    renderRule rule = object
      [ field "id" $ string $ ruleIds M.! rule
      , field "sourceId" $ maybe "null" (string . (sourceIds M.!)) $ rannotation rule
      , field "text" $ string $ show rule
      , field "head" $ string $ show $ rhead rule
      , field "body" $ array $ zipWith renderPredicate [(0 :: Int)..] $ rbody rule
      ]

    renderPredicate :: Int -> Term -> String
    renderPredicate bodyIndex predicate = object
      [ field "index" $ show bodyIndex
      , field "kind" $ string $ predicateKind predicate
      , field "text" $ string $ show predicate
      ]

    renderMatch :: DependencyMatch -> String
    renderMatch details = object
      [ field "dependentRuleId" $ string $ ruleIds M.! dependentRule details
      , field "dependencyRuleId" $ string $ ruleIds M.! dependencyRule details
      , field "predicate" $ string $ show $ dependencyPredicate details
      , field "unifier" $ string $ show $ dependencyUnifier details
      , field "bindings" $ array $ map renderBinding $ toList $ dependencyUnifier details
      ]

field :: String -> String -> String
field key encodedValue = string key ++ ":" ++ encodedValue

object :: [String] -> String
object fields = "{" ++ intercalate "," fields ++ "}"

array :: [String] -> String
array values = "[" ++ intercalate "," values ++ "]"

string :: String -> String
string input = '"' : foldr escape "\"" input
  where
    escape :: Char -> String -> String
    escape '"' rest = '\\' : '"' : rest
    escape '\\' rest = '\\' : '\\' : rest
    escape '\b' rest = '\\' : 'b' : rest
    escape '\f' rest = '\\' : 'f' : rest
    escape '\n' rest = '\\' : 'n' : rest
    escape '\r' rest = '\\' : 'r' : rest
    escape '\t' rest = '\\' : 't' : rest
    escape char rest
      | ord char < 0x20 = "\\u" ++ pad4 (showHex (ord char) "") ++ rest
      | otherwise = char : rest

    pad4 :: String -> String
    pad4 hex = replicate (4 - length hex) '0' ++ hex

predicateKind :: Term -> String
predicateKind term = case toView term of
  TVResolves {} -> "resolves"
  TVNeq {} -> "inequality"
  TVMember {} -> "membership"
  TVRRSIGValid {} -> "signature"
  TVDigestValid {} -> "digest"
  TVNSECUnsigned {} -> "nsec"
  TVNSEC3Unsigned {} -> "nsec3"
  TVClosestEncloser {} -> "closest-encloser"
  _ -> "other"

renderBinding :: (Term, Term) -> String
renderBinding (from, target) = object
  [ field "variable" $ string $ show from
  , field "target" $ string $ show target
  , field "targetKind" $ string $ bindingTargetKind target
  ]

bindingTargetKind :: Term -> String
bindingTargetKind = \case
  TVar {} -> "variable"
  TFunctor {} -> "structured"
  TEmptyList -> "structured"
  TComplex {} -> "dns-data"
  _ -> "value"
