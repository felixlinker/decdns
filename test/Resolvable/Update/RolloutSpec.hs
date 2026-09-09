module Resolvable.Update.RolloutSpec (tests) where

import Control.Monad.Trans.Except (runExcept)
import qualified Data.Map as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=), testCase)

import DNS.Diff (diff)
import DNS.Name (Named (..), root, (<|))
import DNS.Record (Cat (..), RData (..), Type (..))
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile (Zone, insert, isEmpty, singleton, zoneName)
import NameConstraint (compileWildcards)
import Resolvable.Dependencies (resolutionOrder)
import Resolvable.Resolvability (compileConfig)
import Resolvable.Update.Rollout
import Util.Fresh (runFresh)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.Rollout"
    [ testCase "adding one TXT record with no dependencies rolls out immediately" immediateTxtAdd
    , testCase "removing one TXT record with no dependencies rolls out immediately" immediateTxtDelete
    ]

immediateTxtAdd :: Assertion
immediateTxtAdd = do
  let org = "example" <| root
      txt = Named ("www" <| org) (RData IN 300 (TXT "hello"))
      z = singleton (Named org (RRSet.singleton (RData IN 300 (SOA org org 1 2 3 4 5))))
      z' = insert z (fmap RRSet.singleton txt)
      diffs = M.singleton (zoneName z) (diff z z')
  assertBool "expected TXT add diff to be non-empty" $
    maybe False (not . isEmpty) (M.lookup (zoneName z) diffs)
  dz <- dependencies z
  dz' <- dependencies z'
  runExcept (rollout diffs dz dz') @?= Right [flattenDiffs diffs]

immediateTxtDelete :: Assertion
immediateTxtDelete = do
  let org = "example" <| root
      txt = Named ("www" <| org) (RData IN 300 (TXT "hello"))
      z = singleton (Named org (RRSet.singleton (RData IN 300 (SOA org org 1 2 3 4 5))))
      zWithTxt = insert z (fmap RRSet.singleton txt)
      diffs = M.singleton (zoneName zWithTxt) (diff zWithTxt z)
  assertBool "expected TXT delete diff to be non-empty" $
    maybe False (not . isEmpty) (M.lookup (zoneName zWithTxt) diffs)
  dzWithTxt <- dependencies zWithTxt
  dz <- dependencies z
  runExcept (rollout diffs dzWithTxt dz) @?= Right [flattenDiffs diffs]

dependencies :: Zone String -> IO RecordDependencies
dependencies z =
  let ord = runFresh $ fromResolutionOrder . resolutionOrder <$> compileConfig [zoneName z] [(zoneName z, compileWildcards z)]
  in  maybe (fail "cyclic resolution order") return ord
