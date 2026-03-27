module Resolvable.Update.RolloutSpec (tests) where

import qualified Data.Map as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import DNS.Diff (added, deleted, diff)
import DNS.Record (StandardRecord)
import DNS.ZoneFile (Zone, allData, insert, zoneName)
import qualified DNS.ZoneFileSpec as ZS
import Resolvable.Update.Rollout (rollout)
import qualified Util.PartialOrder as PO

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.Rollout"
    [ testProperty "adding one TXT record can be rolled out immediately" propImmediateTxtAdd
    , testProperty "removing one TXT record can be rolled out immediately" propImmediateTxtDelete
    ]

propImmediateTxtAdd :: ZS.ArbZone -> Property
propImmediateTxtAdd (ZS.ArbZone z _) =
  forAll (freshTxtFor z) $ \txt ->
    let z' = insert z txt
        diffs = M.singleton (zoneName z) (diff z z')
    in rollout diffs PO.empty PO.empty === [[added txt]]

propImmediateTxtDelete :: ZS.ArbZone -> Property
propImmediateTxtDelete (ZS.ArbZone z _) =
  forAll (freshTxtFor z) $ \txt ->
    let zWithTxt = insert z txt
        diffs = M.singleton (zoneName zWithTxt) (diff zWithTxt z)
    in rollout diffs PO.empty PO.empty === [[deleted txt]]

freshTxtFor :: Zone String -> Gen StandardRecord
freshTxtFor z =
  ZS.genTXTRecord (zoneName z) `suchThat` (`notElem` allData z)
