{-# LANGUAGE LambdaCase #-}
module Resolvable.ResolvabilitySpec (tests) where

import qualified Data.Set as S
import Data.List (tails)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck

import DNS.ZoneFileSpec (ArbZone(..))
import DNS.ZoneFile (zoneName)
import NameConstraint (compileWildcards)
import Resolvable.Prolog.Core
import Resolvable.Resolvability (fromZone)
import Util.Fresh (runFresh)

tests :: TestTree
tests =
  localOption (QuickCheckTests 10) $
  testGroup
    "Resolvable.Resolvability"
    [ testProperty "rules compiled from a zone have pairwise distinct variables" propDistinctRuleVariables
    ]

propDistinctRuleVariables :: ArbZone -> Property
propDistinctRuleVariables (ArbZone z _) =
  conjoin
    [ counterexample
        (unlines
          [ "rules share variables " ++ show (S.toList shared)
          , show r1
          , show r2
          ])
        (S.null shared)
    | r1 : rest <- tails (S.toList rules)
    , r2 <- rest
    , let shared = variables r1 `S.intersection` variables r2
    ]
  where
    rules = runFresh $ fromZone (list [nameTerm $ zoneName z]) (compileWildcards z)

variables :: Rule a -> S.Set String
variables (Rule _ h b) = S.unions $ map termVariables (h : b)

termVariables :: Term -> S.Set String
termVariables = walk (\case
  TVar v -> S.singleton v
  _ -> S.empty)
