module Resolvable.Update.RolloutSpec (tests) where

import qualified Data.Map as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck

import DNS.Diff (diff)
import DNS.Name (Named (..), root, (<|))
import DNS.Record (Cat (..), RData (..), StandardRecord, Type (..))
import qualified DNS.RRSet as RRSet
import DNS.ZoneFile (Zone, allRecords, insert, isEmpty, singleton, zoneName)
import qualified DNS.ZoneFileSpec as ZS
import NameConstraint (compileWildcards)
import Resolvable.Update.Rollout (diffServers, rollout)
import qualified Util.PartialOrder as PO

tests :: TestTree
tests =
  testGroup
    "Resolvable.Update.Rollout"
    [ testCase "diffServers reports changes in non-root zones" reportsNonRootZoneDiff
      --   testProperty "adding one TXT record can be rolled out immediately" propImmediateTxtAdd
      -- , testProperty "removing one TXT record can be rolled out immediately" propImmediateTxtDelete
    ]

reportsNonRootZoneDiff :: Assertion
reportsNonRootZoneDiff = do
  let ns = "ns" <| ("example" <| root)
      owner = "www" <| ("example" <| root)
      zone = singleton (Named owner (RRSet.singleton (RData IN 300 (A "192.0.2.1"))))
      diffs = diffServers [(ns, [compileWildcards zone])] []
  assertBool "expected deletion diff for non-root zone" $
    maybe False (not . isEmpty) (M.lookup ns diffs)

-- propImmediateTxtAdd :: ZS.ArbZone -> Property
-- propImmediateTxtAdd (ZS.ArbZone z _) =
--   forAll (freshTxtFor z) $ \txt ->
--     let z' = insert z (fmap RRSet.singleton txt)
--         diffs = M.singleton (zoneName z) (diff z z')
--     in rollout diffs PO.empty PO.empty === Just [diffs]

-- propImmediateTxtDelete :: ZS.ArbZone -> Property
-- propImmediateTxtDelete (ZS.ArbZone z _) =
--   forAll (freshTxtFor z) $ \txt ->
--     let zWithTxt = insert z (fmap RRSet.singleton txt)
--         diffs = M.singleton (zoneName zWithTxt) (diff zWithTxt z)
--     in rollout diffs PO.empty PO.empty === Just [diffs]

freshTxtFor :: Zone String -> Gen StandardRecord
freshTxtFor z =
  ZS.genTXTRecord (zoneName z) `suchThat` (`notElem` allRecords z)
