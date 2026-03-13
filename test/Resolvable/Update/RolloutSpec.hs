module Resolvable.Update.RolloutSpec (tests) where

import qualified Data.Map as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import DNS.Diff (added, deleted, diff)
import DNS.Record (StandardRecord)
import DNS.ZoneFile (Zone, allData, insert, zoneName)
import qualified DNS.ZoneFile.PrintingSpec as PS
import Resolvable.Dependencies (empty)
import Resolvable.Update.Rollout (rollout)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.Rollout"
    [ testProperty "adding one TXT record can be rolled out immediately" propImmediateTxtAdd
    , testProperty "removing one TXT record can be rolled out immediately" propImmediateTxtDelete
    ]

propImmediateTxtAdd :: PS.ArbZone -> Property
propImmediateTxtAdd (PS.ArbZone z _) =
  forAll (freshTxtFor z) $ \txt ->
    let z' = insert z txt
        diffs = M.singleton (zoneName z) (diff z z')
    in rollout diffs empty empty === [[added txt]]

propImmediateTxtDelete :: PS.ArbZone -> Property
propImmediateTxtDelete (PS.ArbZone z _) =
  forAll (freshTxtFor z) $ \txt ->
    let zWithTxt = insert z txt
        diffs = M.singleton (zoneName zWithTxt) (diff zWithTxt z)
    in rollout diffs empty empty === [[deleted txt]]

freshTxtFor :: Zone String -> Gen StandardRecord
freshTxtFor z =
  PS.genTXTRecord (zoneName z) `suchThat` (`notElem` allData z)
