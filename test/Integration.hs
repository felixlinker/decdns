module Integration (tests) where

import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S

import DNS.Diff (hasChanges)
import DNS.Name (Name, root, (<|))
import DNS.NameSpec (mkName)
import qualified DNS.RecordType as RT
import DNS.ZoneFile (allRecords, isEmpty, zoneName)
import NameConstraint
  ( ConstrainedLabel
  , LabelConstraint (Wildcard)
  , exactly
  )
import Parsing.Config (parseZoneFileZones, parseZoneFiles)
import DNS.Wellformedness (zoneWellformed)
import Resolvable.Dependencies (ConstrainedQuery (CQuery), computeResolvable, queryFromRule)
import Resolvable.Prolog.Core (Term, ad, nameTerm, resolves, top, ttypeTerm, wildcard, wildcards4)
import Resolvable.Resolvability (NameServersConfig, compileConfig, plResolvable)
import Resolvable.Update.Dependencies (computeRegressions)
import Resolvable.Update.Rollout (NSDiffs, diffServers)
import qualified Resolvable.ConstrainedUnification as CU
import qualified Resolvable.Unification as U
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Util.Fresh (runFresh)
import Util.Monad (failLeft, handleError)

tests :: TestTree
tests =
  testGroup
    "Examples"
    [ testGroup "Parsing/DNS.ZoneFile/Wellformedness" $
        map exampleFileTest exampleFiles
    , testGroup "Resolvable.Resolvability" $
        map exampleConfigTest exampleConfigs
    , testGroup
        "Resolvable.Resolvability authentication"
        [ testCase "signed authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest "signed" signedFiles
        , testCase "signed-nsec3 authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest "signed-nsec3" signedNsec3Files
        , testCase "partially-signed authenticates unsigned child zone through NSEC" $
            authenticatedUnsignedDelegationTest "partially-signed" partiallySignedFiles
        , testCase "partially-signed-nsec3 authenticates unsigned child zone through NSEC3 owner proof" $
            authenticatedUnsignedDelegationTest "partially-signed-nsec3" partiallySignedNsec3Files
        , testCase "partially-signed-nsec3-oo filters invalid NSEC3 unsigned-delegation proof" $
            doesNotAuthenticateUnsignedDelegationTest "partially-signed-nsec3-oo" partiallySignedNsec3OOFiles
        , testCase "signed does not authenticate the child zone as unsigned through NSEC" $
            signedDelegationDoesNotUseUnsignedProofTest "signed" signedFiles
        , testCase "signed-nsec3 does not authenticate the child zone as unsigned through NSEC3" $
            signedDelegationDoesNotUseUnsignedProofTest "signed-nsec3" signedNsec3Files
        , testCase "base does not authenticate unsigned answers" $ do
            results <- resolvableQueries baseFiles
            assertResolutions "base" results []
              [ authenticated nsCom RT.A aCom
              , authenticated nsCom RT.A bCom
              , authenticated nsCom RT.CNAME aCom
              , authenticated nsCom RT.NS bCom
              , authenticated nsBCom RT.A bCom ] ]
    , testGroup
        "Resolvable.Update.Dependencies"
        [ testCase "base to new has expected unsigned regression facts" $
            regressionTest "new" newFiles
              [ Query nsCom RT.CNAME aCom
              , Query nsCom RT.NS aCom
              , Query nsCom RT.NS bCom
              , Query nsCom RT.SOA aCom
              , Query nsCom RT.SOA bCom ]
              [ Query nsCom RT.SOA com ]
        , testCase "base to broken has expected unsigned regression facts" $
            regressionTest "broken" brokenFiles
              [ Query nsCom RT.A aCom
              , Query nsCom RT.A bCom
              , Query nsCom RT.A nsBCom
              , Query nsCom RT.NS aCom
              , Query nsCom RT.NS bCom
              , Query nsCom RT.SOA aCom
              , Query nsCom RT.SOA bCom ]
              [ Query nsCom RT.CNAME aCom
              , Query nsCom RT.SOA com ] ]
    , testGroup
        "DNS.Diff/Resolvable.Update.Rollout.diffServers"
        [ testCase "base to new has expected record-level changes" $
            diffTest "new" newFiles
              [ Changed nsCom (exactly aCom)
              , Changed nsCom (Wildcard [] <| exactly com)
              , ServerChanged nsBCom ]
              [ Unchanged nsCom (exactly net) ]
        , testCase "base to broken has expected record-level changes" $
            diffTest "broken" brokenFiles
              [ Changed nsCom (exactly bCom)
              , Changed nsCom (exactly nsBCom)
              , ServerChanged nsBCom ]
              [ Unchanged nsCom (exactly aCom)
              , Unchanged nsCom (exactly net) ] ] ]

data ExampleFile = ExampleFile
  { examplePath :: FilePath
  , expectedOrigin :: Name String
  , expectedRecordCount :: Int
  }

exampleFiles :: [ExampleFile]
exampleFiles =
  [ ExampleFile "examples/cname/base/ns.com." com 5
  , ExampleFile "examples/cname/base/ns.b.com." bCom 4
  , ExampleFile "examples/cname/new/ns.com." com 3
  , ExampleFile "examples/cname/broken/ns.com." com 3
  , ExampleFile "examples/cname/partially-signed/ns.com." com 18
  , ExampleFile "examples/cname/partially-signed/ns.b.com." bCom 4
  , ExampleFile "examples/cname/signed/ns.com." com 20
  , ExampleFile "examples/cname/signed/ns.b.com." bCom 16
  , ExampleFile "examples/cname/signed-non-zone-key/ns.com." com 20
  , ExampleFile "examples/cname/signed-non-zone-key/ns.b.com." bCom 17
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.com." com 20
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.b.com." bCom 4
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.com." com 18
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.b.com." bCom 4
  , ExampleFile "examples/cname/signed-nsec3/ns.com." com 22
  , ExampleFile "examples/cname/signed-nsec3/ns.b.com." bCom 18
  , ExampleFile "examples/ns/self-deleg/ns.com." com 6
  , ExampleFile "examples/ns/cyclic-deleg/root.net." dnsRoot 8
  , ExampleFile "examples/ns/cyclic-deleg/ns.net." net 5 ]

data ExampleConfig = ExampleConfig
  { configName :: String
  , configFiles :: [FilePath]
  , expectedQueries :: [Query]
  , unexpectedQueries :: [Query]
  }

exampleConfigs :: [ExampleConfig]
exampleConfigs =
  [ ExampleConfig
      "base"
      baseFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.NS aCom
      , Query nsCom RT.NS bCom
      , Query nsCom RT.SOA aCom
      , Query nsCom RT.SOA bCom
      , Query nsBCom RT.A bCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "new"
      newFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.A nsBCom
      , Query nsCom RT.SOA com ]
      [ Query nsCom RT.CNAME aCom
      , Query nsCom RT.NS bCom ]
  , ExampleConfig
      "broken"
      brokenFiles
      [ Query nsCom RT.CNAME aCom
      , Query nsCom RT.SOA com ]
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.NS bCom ]
  , ExampleConfig
      "partially-signed"
      partiallySignedFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.NSEC aCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "signed"
      signedFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.DNSKEY bCom
      , Query nsCom RT.DS bCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "signed-non-zone-key"
      signedNonZoneKeyFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.DNSKEY bCom
      , Query nsCom RT.DS bCom ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "partially-signed-nsec3"
      partiallySignedNsec3Files
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.NSEC3PARAM com ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "partially-signed-nsec3-oo"
      partiallySignedNsec3OOFiles
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.NSEC3PARAM com ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "signed-nsec3"
      signedNsec3Files
      [ Query nsCom RT.A aCom
      , Query nsCom RT.A bCom
      , Query nsCom RT.CNAME aCom
      , Query nsCom RT.DNSKEY com
      , Query nsCom RT.DNSKEY bCom
      , Query nsCom RT.DS bCom
      , Query nsCom RT.NSEC3PARAM com ]
      [Query nsCom RT.TXT aCom]
  , ExampleConfig
      "ns/self-deleg"
      selfDelegFiles
      [ Query nsCom RT.NS com
      , Query nsCom RT.SOA com
      , Query nsCom RT.A nsCom
      , Query nsCom RT.NS subACom
      , Query nsCom RT.SOA subACom
      , Query nsCom RT.A subACom ]
      [Query nsCom RT.TXT subACom]
  , ExampleConfig
      "ns/cyclic-deleg"
      cyclicDelegFiles
      [ Query rootNet RT.NS dnsRoot
      , Query rootNet RT.SOA dnsRoot
      , Query rootNet RT.NS net
      , Query rootNet RT.A nsNet
      , Query nsNet RT.NS net
      , Query nsNet RT.SOA net
      , Query nsNet RT.NS rootNet
      , Query nsNet RT.A rootNet
      , Query rootNet RT.NS rootNet
      , Query rootNet RT.SOA rootNet
      , Query rootNet RT.A rootNet
      , Query nsNet RT.A subRootNet ]
      [Query rootNet RT.TXT subRootNet] ]

baseFiles, newFiles, brokenFiles, partiallySignedFiles, signedFiles, signedNonZoneKeyFiles :: [FilePath]
cyclicDelegFiles, selfDelegFiles :: [FilePath]
partiallySignedNsec3Files, partiallySignedNsec3OOFiles, signedNsec3Files :: [FilePath]
baseFiles =
  [ "examples/cname/base/ns.com."
  , "examples/cname/base/ns.b.com." ]

newFiles = ["examples/cname/new/ns.com."]

brokenFiles = ["examples/cname/broken/ns.com."]

partiallySignedFiles =
  [ "examples/cname/partially-signed/ns.com."
  , "examples/cname/partially-signed/ns.b.com." ]

signedFiles =
  [ "examples/cname/signed/ns.com."
  , "examples/cname/signed/ns.b.com." ]

signedNonZoneKeyFiles =
  [ "examples/cname/signed-non-zone-key/ns.com."
  , "examples/cname/signed-non-zone-key/ns.b.com." ]

partiallySignedNsec3Files =
  [ "examples/cname/partially-signed-nsec3/ns.com."
  , "examples/cname/partially-signed-nsec3/ns.b.com." ]

partiallySignedNsec3OOFiles =
  [ "examples/cname/partially-signed-nsec3-oo/ns.com."
  , "examples/cname/partially-signed-nsec3-oo/ns.b.com." ]

signedNsec3Files =
  [ "examples/cname/signed-nsec3/ns.com."
  , "examples/cname/signed-nsec3/ns.b.com." ]

selfDelegFiles = ["examples/ns/self-deleg/ns.com."]

cyclicDelegFiles =
  [ "examples/ns/cyclic-deleg/root.net."
  , "examples/ns/cyclic-deleg/ns.net." ]

data Query = Query (Name String) RT.Type (Name String)
  deriving Eq

instance Show Query where
  show (Query ns rtype qname) =
    show ns ++ " " ++ show rtype ++ " " ++ show qname

data Resolution = Resolution Query Term Term
  deriving Eq

instance Show Resolution where
  show (Resolution query verification auth) =
    show query ++ " verified=" ++ show verification ++ " auth=" ++ show auth

authenticated :: Name String -> RT.Type -> Name String -> Resolution
authenticated ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 1)

authenticatedUnsigned :: Name String -> RT.Type -> Name String -> Resolution
authenticatedUnsigned ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 0)

dnsRoot, com, net, aCom, bCom, subACom :: Name String
rootNet, subRootNet, nsCom, nsBCom, nsNet :: Name String
dnsRoot = root
com = mkName ["com", ""]
net = mkName ["net", ""]
aCom = mkName ["a", "com", ""]
bCom = mkName ["b", "com", ""]
subACom = mkName ["sub", "a", "com", ""]
rootNet = mkName ["root", "net", ""]
subRootNet = mkName ["sub", "root", "net", ""]
nsCom = mkName ["ns", "com", ""]
nsBCom = mkName ["ns", "b", "com", ""]
nsNet = mkName ["ns", "net", ""]

rootServers :: [Name String]
rootServers = [nsCom, rootNet]

exampleFileTest :: ExampleFile -> TestTree
exampleFileTest example =
  testCase (examplePath example) $ do
    config <- parseZoneFileZones zoneWellformed (examplePath example)
    zone <- case config of
      [(_, parsedZone)] -> return parsedZone
      _ -> assertFailure (examplePath example ++ ": expected exactly one name server")
    zoneName zone @?= exactly (expectedOrigin example)
    length (allRecords zone) @?= expectedRecordCount example

exampleConfigTest :: ExampleConfig -> TestTree
exampleConfigTest example =
  testCase (configName example) $ do
    results <- resolvableQueries (configFiles example)
    assertQueries
      (configName example)
      results
      (expectedQueries example)
      (unexpectedQueries example)

authenticatedResolutionTest :: String -> [FilePath] -> Assertion
authenticatedResolutionTest label files = do
  results <- resolvableQueries files
  assertResolutions label results
    [ authenticated nsCom RT.A aCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.CNAME aCom
    , authenticated nsCom RT.DNSKEY com
    , authenticated nsCom RT.DNSKEY bCom
    , authenticated nsCom RT.DS bCom ]
    []

authenticatedUnsignedDelegationTest :: String -> [FilePath] -> Assertion
authenticatedUnsignedDelegationTest label files = do
  results <- resolvableQueries files
  assertResolutions label results
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom ]
    [ authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]

signedDelegationDoesNotUseUnsignedProofTest :: String -> [FilePath] -> Assertion
signedDelegationDoesNotUseUnsignedProofTest label files = do
  results <- resolvableQueries files
  assertResolutions label results
    [ authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom ]

doesNotAuthenticateUnsignedDelegationTest :: String -> [FilePath] -> Assertion
doesNotAuthenticateUnsignedDelegationTest label files = do
  results <- resolvableQueries files
  assertResolutions label results []
    [ authenticatedUnsigned nsCom RT.A bCom
    , authenticatedUnsigned nsCom RT.A nsBCom
    , authenticated nsCom RT.A bCom
    , authenticated nsCom RT.A nsBCom
    , authenticated nsCom RT.DS bCom ]

regressionTest :: String -> [FilePath] -> [Query] -> [Query] -> Assertion
regressionTest label newConfig expected unexpected = do
  old <- loadConfig baseFiles
  new <- loadConfig newConfig
  regressions <- failLeft $
    runFresh $
      runExceptT $
        computeRegressions
          (plResolvable =<< compileConfig rootServers old)
          (plResolvable =<< compileConfig rootServers new)
  assertQueries ("base -> " ++ label) (S.fromList regressions) expected unexpected

data DiffExpectation
  = Changed (Name String) (Name ConstrainedLabel)
  | Unchanged (Name String) (Name ConstrainedLabel)
  | ServerChanged (Name String)

diffTest :: String -> [FilePath] -> [DiffExpectation] -> [DiffExpectation] -> Assertion
diffTest _label newConfig expected unexpected = do
  old <- loadConfig baseFiles
  new <- loadConfig newConfig
  let diffs = diffServers old new
  forM_ expected (assertDiffExpectation diffs)
  forM_ unexpected (assertDiffExpectation diffs)

assertDiffExpectation :: NSDiffs -> DiffExpectation -> Assertion
assertDiffExpectation diffs expectation =
  case expectation of
    Changed ns name ->
      assertBool
        ("expected record changes on " ++ show ns ++ " at " ++ show name)
        (hasChange ns name diffs)
    Unchanged ns name ->
      assertBool
        ("did not expect record changes on " ++ show ns ++ " at " ++ show name)
        (not (hasChange ns name diffs))
    ServerChanged ns ->
      assertBool
        ("expected server changes on " ++ show ns)
        (maybe False (not . isEmpty) (M.lookup ns diffs))

hasChange :: Name String -> Name ConstrainedLabel -> NSDiffs -> Bool
hasChange ns name diffs =
  maybe False (hasChanges name) (M.lookup ns diffs)

assertQueries :: String -> S.Set ConstrainedQuery -> [Query] -> [Query] -> Assertion
assertQueries label results expected unexpected = do
  forM_ expected $ \query ->
    assertBool
      (label ++ " should include " ++ show query ++ "\n" ++ showResults results)
      (any (coversQuery query) results)
  forM_ unexpected $ \query ->
    assertBool
      (label ++ " should not include " ++ show query ++ "\n" ++ showResults results)
      (not (any (coversQuery query) results))

assertResolutions :: String -> S.Set ConstrainedQuery -> [Resolution] -> [Resolution] -> Assertion
assertResolutions label results expected unexpected = do
  forM_ expected $ \resolution ->
    assertBool
      (label ++ " should include " ++ show resolution ++ "\n" ++ showResults results)
      (any (coversResolution resolution) results)
  forM_ unexpected $ \resolution ->
    assertBool
      (label ++ " should not include " ++ show resolution ++ "\n" ++ showResults results)
      (not (any (coversResolution resolution) results))

showResults :: S.Set ConstrainedQuery -> String
showResults = unlines . map show . S.toList

coversQuery :: Query -> ConstrainedQuery -> Bool
coversQuery = coversTerm . queryTerm

coversResolution :: Resolution -> ConstrainedQuery -> Bool
coversResolution = coversTerm . resolutionTerm

coversTerm :: Term -> ConstrainedQuery -> Bool
coversTerm expected (CQuery actual constraints) =
  isJust $
    CU.extendUnifier
      actual
      expected
      (CU.CUnifier U.identity constraints)

queryTerm :: Query -> Term
queryTerm (Query ns rtype qname) = runFresh $ do
  (wc1, wc2, wc3, wc4) <- wildcards4
  return $ resolves (nameTerm ns) (ttypeTerm rtype) (nameTerm qname) wc1 wc2 wc3 wc4

resolutionTerm :: Resolution -> Term
resolutionTerm (Resolution (Query ns rtype qname) verification auth) = runFresh $ do
  wc1 <- wildcard
  wc2 <- wildcard
  return $ resolves (nameTerm ns) (ttypeTerm rtype) (nameTerm qname) wc1 wc2 verification auth

resolvableQueries :: [FilePath] -> IO (S.Set ConstrainedQuery)
resolvableQueries files = do
  cfg <- loadConfig files
  let rules = runFresh $ plResolvable =<< compileConfig rootServers cfg
  resolvableRules <- handleError (computeResolvable True rules)
  qs <- handleError (traverse queryFromRule $ S.toList resolvableRules)
  return $ S.fromList qs

loadConfig :: [FilePath] -> IO NameServersConfig
loadConfig = parseZoneFiles zoneWellformed
