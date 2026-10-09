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
import NameConstraint (ConstrainedRRSet, toStandardRecord)
import Resolvable.Dependencies
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Resolvability (NameServersConfig, RComp(..))
import Resolvable.Unification (toList)

renderVisualization :: [Name String] -> [FilePath] -> NameServersConfig -> S.Set (Rule RComp) -> String
renderVisualization roots zonePaths _config rules = object
  [ field "version" "2"
  , field "rootServers" $ array $ map (string . show) roots
  , field "zoneFiles" $ array $ map string zonePaths
  , field "sources" $ array $ map renderSource sources
  , field "rules" $ array $ map renderRule ruleList
  , field "matches" $ array $ map renderMatch matchList
  ]
  where
    sources :: [(Name String, ConstrainedRRSet)]
    sources = S.toList $ S.fromList $ map (sourceOf . rannotation) ruleList

    ruleList :: [Rule RComp]
    ruleList = S.toList rules

    sourceIds :: M.Map (Name String, ConstrainedRRSet) String
    sourceIds = M.fromList $ zip sources (map (("source-" ++) . show) [(1 :: Int)..])

    ruleIds :: M.Map (Rule RComp) String
    ruleIds = M.fromList $ zip ruleList (map (("rule-" ++) . show) [(1 :: Int)..])

    matchList :: [DependencyMatch]
    matchList = dependencyMatches rules

    sourceOf :: RComp -> (Name String, ConstrainedRRSet)
    sourceOf source = (fromNS source, fromRecord source)

    renderSource :: (Name String, ConstrainedRRSet) -> String
    renderSource source@(ns, Named owner records) = object
      [ field "id" $ string $ sourceIds M.! source
      , field "server" $ string $ show ns
      , field "owner" $ string $ show owner
      , field "recordType" $ string $ show $ toType $ rtype $ NE.head records
      , field "records" $ array $ map (string . show . toStandardRecord . Named owner) $ NE.toList records
      , field "ruleIds" $ array $ map string $ rulesFor source
      ]

    rulesFor :: (Name String, ConstrainedRRSet) -> [String]
    rulesFor source = mapMaybe (ruleFor source) ruleList

    ruleFor :: (Name String, ConstrainedRRSet) -> Rule RComp -> Maybe String
    ruleFor source rule
      | sourceOf (rannotation rule) == source = M.lookup rule ruleIds
      | otherwise = Nothing

    renderRule :: Rule RComp -> String
    renderRule rule = object
      [ field "id" $ string $ ruleIds M.! rule
      , field "sourceId" $ string $ sourceIds M.! sourceOf (rannotation rule)
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
