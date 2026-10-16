module Integration (tests) where

import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (isJust)
import Data.List (isPrefixOf, nub, sort)
import qualified Data.Set as S
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, (</>))

import DNS.Name (Name, root, toStringName)
import DNS.NameSpec (mkName)
import DNS.Record (Type (DNAME, NS, SOA))
import DNS.RRSet (RRSet)
import qualified DNS.RecordType as RT
import DNS.Wellformedness (zoneWellformed)
import DNS.Zone (splitZone)
import DNS.ZoneFile (zoneName, Zone)
import NameConstraint (compileName, ConstrainedLabel)
import Parsing (parseZoneFiles, parseDS)
import Resolvable.Consequences
import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving (getSolved)
import Resolvable.Prolog.Core (Term (TComplex), ad, bot, list, nameTerm, referralZone, resolvable, resolves, top, ttypeTerm, wildcard, wildcards3)
import Resolvable.Resolvability (compileConfig)
import Resolvable.Update.Dependencies (computeRegressions)
import qualified Resolvable.Unification as U
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Util.Fresh (runFresh)
import Util.Monad (failLeft)

tests :: TestTree
tests =
  testGroup
    "Examples"
    [ testGroup "Parsing/DNS.ZoneFile/Wellformedness" $
        map exampleFileTest exampleFiles
    , testGroup "resolvability"
        (map exampleConfigTest exampleConfigs ++
          [ testCase "handles an NS delegation loop" $ do
              exactResolutionTest nsLoopFiles
                [ resolves (list [nameTerm nsCom]) (ttypeTerm RT.NS) referralZone (nameTerm com)
                    (list [TComplex (NS rootNet)]) bot (ad 0)
                , resolves (list [nameTerm rootNet]) (ttypeTerm RT.NS) referralZone (nameTerm com)
                    (list [TComplex (NS nsCom)]) bot (ad 0) ]
              misconfigurationLoopTest nsLoopFiles [] [Query dnsRoot RT.A aCom]
          , testCase "handles a CNAME loop" $
              misconfigurationLoopTest cnameLoopFiles
                [Query com RT.CNAME aCom, Query com RT.CNAME bCom]
                [Query com RT.A aCom, Query com RT.A bCom]
          , testCase "handles an empty DNAME loop" $
              exactResolutionTest dnameLoopEmptyFiles
                [ resolves (list [nameTerm nsCom]) (ttypeTerm RT.DNAME) (nameTerm com) (nameTerm aaCom)
                    (list [TComplex (DNAME bCom)]) bot (ad 0)
                , resolves (list [nameTerm nsCom]) (ttypeTerm RT.DNAME) (nameTerm com) (nameTerm bCom)
                    (list [TComplex (DNAME aCom)]) bot (ad 0)
                , resolves (list [nameTerm nsCom]) (ttypeTerm RT.NS) (nameTerm com) (nameTerm com)
                    (list [TComplex (NS nsCom)]) bot (ad 0)
                , resolves (list [nameTerm nsCom]) (ttypeTerm RT.SOA) (nameTerm com) (nameTerm com)
                    (list [TComplex (SOA nsCom exampleCom 0 0 0 0 0)]) bot (ad 0)
                ]
          ])
    , testGroup
        "resolvability authentication"
        [ testCase "signed authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest signedFiles
        , testCase "signed facts have fully reduced constraints" $ do
            results <- resolvableQueries signedFiles
            forM_ results $ \(Fact _ constraints) ->
              fmap Store.toBody (getSolved (Store.simplify constraints))
                @?= Just (Store.toBody constraints)
        , testCase "signed authenticates a TXT record" $ do
            results <- resolvableQueries signedTxtFiles
            assertResolutions signedTxtFiles results
              [authenticated com RT.TXT com]
              []
        , testCase "signed authenticates an apex NS RRset" $ do
            results <- resolvableQueries signedNsFiles
            assertResolutions signedNsFiles results
              [ authenticated exampleZone RT.NS exampleZone
              , authenticated exampleZone RT.NS exampleZone]
              []
        , testCase "signed-new authenticates answers synthesized from a wildcard" $
            authenticatedWildcardResolutionTest signedNewFiles
        , testCase "signed wildcard records authenticate synthesized answers" $ do
            results <- resolvableQueries signedWildcardFiles
            assertResolutions signedWildcardFiles results
              [authenticated com RT.A aCom]
              []
        , testCase "signed records with asterisk labels in owner names authenticate" $ do
            results <- resolvableQueries signedWildcardFiles
            assertResolutions signedWildcardFiles results
              [authenticated com RT.A xWildcardCom]
              []
        , testCase "signed-nsec3 authenticates the CNAME chain and child zone" $
            authenticatedResolutionTest signedNsec3Files
        , testCase "partially-signed authenticates unsigned child zone through NSEC" $
            authenticatedUnsignedDelegationTest partiallySignedFiles
        , testCase "partially-signed-nsec3 authenticates unsigned child zone through NSEC3 owner proof" $
            authenticatedUnsignedDelegationTest partiallySignedNsec3Files
        , testCase "partially-signed-nsec3-oo filters invalid NSEC3 unsigned-delegation proof" $
            doesNotAuthenticateUnsignedDelegationTest partiallySignedNsec3OOFiles
        , testCase "signed does not authenticate the child zone as unsigned through NSEC" $
            signedDelegationDoesNotUseUnsignedProofTest signedFiles
        , testCase "signed-nsec3 does not authenticate the child zone as unsigned through NSEC3" $
            signedDelegationDoesNotUseUnsignedProofTest signedNsec3Files
        , testCase "base does not authenticate unsigned answers" $ do
            results <- resolvableQueries baseFiles
            assertResolutions baseFiles results []
              [ authenticated com RT.A aCom
              , authenticated com RT.A bCom
              , authenticated com RT.CNAME aCom
              , authenticated com RT.NS bCom
              , authenticated bCom RT.A bCom ] ]
    , testGroup
        "Resolvable.Update.Dependencies"
        [ testCase "base to new has expected unsigned regression facts" $
            regressionTest newFiles
              []
              [ Query com RT.A aCom
              , Query com RT.A bCom
              , Query com RT.A nsBCom ]
        , testCase "base to broken has expected unsigned regression facts" $
            regressionTest brokenFiles
              [ Query com RT.A aCom
              , Query com RT.A bCom
              , Query com RT.A nsBCom ]
              [] ] ]

data ExampleFile = ExampleFile
  { examplePath :: FilePath
  , expectedZones :: [Name String]
  }

exampleFiles :: [ExampleFile]
exampleFiles =
  [ ExampleFile "examples/cname/base/ns.com." [com]
  , ExampleFile "examples/cname/base/ns.b.com." [bCom]
  , ExampleFile "examples/cname/new/ns.com." [com]
  , ExampleFile "examples/cname/broken/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed/ns.b.com." [bCom]
  , ExampleFile "examples/cname/signed/ns.com." [com]
  , ExampleFile "examples/cname/signed/ns.b.com." [bCom]
  , ExampleFile "examples/cname/signed-new/ns.com." [com]
  , ExampleFile "examples/cname/signed-non-zone-key/ns.com." [com]
  , ExampleFile "examples/cname/signed-non-zone-key/ns.b.com." [bCom]
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed-nsec3/ns.b.com." [bCom]
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.com." [com]
  , ExampleFile "examples/cname/partially-signed-nsec3-oo/ns.b.com." [bCom]
  , ExampleFile "examples/cname/signed-nsec3/ns.com." [com]
  , ExampleFile "examples/cname/signed-nsec3/ns.b.com." [bCom]
  , ExampleFile "examples/dnssec/txt/ns.com." [com]
  , ExampleFile "examples/dnssec/ns/example." [exampleZone]
  , ExampleFile "examples/dnssec/wildcard/ns.com." [com]
  , ExampleFile "examples/ns/self-deleg/ns.com." [com, subACom]
  , ExampleFile "examples/ns/sub-self-deleg/root.net." [dnsRoot, rootNet]
  , ExampleFile "examples/ns/sub-self-deleg/ns.net." [net]
  , ExampleFile "examples/ns/cyclic-deleg/root.net." [dnsRoot]
  , ExampleFile "examples/ns/cyclic-deleg/ns.net." [net]
  , ExampleFile "examples/ns/cyclic-deleg/root.com." [dnsRoot]
  , ExampleFile "examples/dname/loop-empty/ns.net." [com]
  , ExampleFile "examples/dname/loop-non-empty/ns.net." [com] ]

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
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.NS aCom
      , Query com RT.NS bCom
      , Query com RT.SOA aCom
      , Query com RT.SOA bCom
      , Query bCom RT.A bCom ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "new"
      newFiles
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.A nsBCom
      , Query com RT.SOA com ]
      [ Query com RT.CNAME aCom
      , Query com RT.NS bCom ]
  , ExampleConfig
      "broken"
      brokenFiles
      [ Query com RT.CNAME aCom
      , Query com RT.SOA com ]
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.NS bCom ]
  , ExampleConfig
      "partially-signed"
      partiallySignedFiles
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.DNSKEY com
      , Query com RT.NSEC aCom ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "signed"
      signedFiles
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.DNSKEY com
      , Query com RT.DNSKEY bCom
      , Query com RT.DS bCom ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "signed-non-zone-key"
      signedNonZoneKeyFiles
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.DNSKEY com
      , Query com RT.DNSKEY bCom
      , Query com RT.DS bCom ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "partially-signed-nsec3"
      partiallySignedNsec3Files
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.DNSKEY com
      , Query com RT.NSEC3PARAM com ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "partially-signed-nsec3-oo"
      partiallySignedNsec3OOFiles
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.DNSKEY com
      , Query com RT.NSEC3PARAM com ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "signed-nsec3"
      signedNsec3Files
      [ Query com RT.A aCom
      , Query com RT.A bCom
      , Query com RT.CNAME aCom
      , Query com RT.DNSKEY com
      , Query com RT.DNSKEY bCom
      , Query com RT.DS bCom
      , Query com RT.NSEC3PARAM com ]
      [Query com RT.TXT aCom]
  , ExampleConfig
      "dnssec/ns"
      signedNsFiles
      [Query exampleZone RT.NS exampleZone]
      []
  , ExampleConfig
      "ns/self-deleg"
      selfDelegFiles
      [ Query com RT.NS com
      , Query com RT.SOA com
      , Query com RT.A nsCom
      , Query com RT.NS subACom
      , Query com RT.SOA subACom
      , Query com RT.A subACom ]
      [Query com RT.TXT subACom]
  , ExampleConfig
      "ns/sub-self-deleg"
      subSelfDelegFiles
      [ Query dnsRoot RT.NS dnsRoot
      , Query dnsRoot RT.SOA dnsRoot
      , Query dnsRoot RT.NS net
      , Query dnsRoot RT.A nsNet
      , Query net RT.NS net
      , Query net RT.SOA net
      , Query net RT.NS rootNet
      , Query net RT.A rootNet
      , Query dnsRoot RT.NS rootNet
      , Query dnsRoot RT.SOA rootNet
      , Query dnsRoot RT.A rootNet
      , Query net RT.A subRootNet ]
      [Query dnsRoot RT.TXT subRootNet]
  , ExampleConfig
      "dname/loop-non-empty"
      dnameLoopNonEmptyFiles
      [Query com RT.TXT aSubSubSubCom]
      [] ]

baseFiles, newFiles, brokenFiles, partiallySignedFiles, signedFiles, signedNewFiles, signedNonZoneKeyFiles :: [FilePath]
selfDelegFiles, subSelfDelegFiles, nsLoopFiles, cnameLoopFiles :: [FilePath]
dnameLoopEmptyFiles, dnameLoopNonEmptyFiles :: [FilePath]
partiallySignedNsec3Files, partiallySignedNsec3OOFiles, signedNsec3Files, signedNsFiles, signedTxtFiles, signedWildcardFiles :: [FilePath]
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

signedNewFiles = ["examples/cname/signed-new/ns.com."]

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

signedNsFiles = ["examples/dnssec/ns/example."]

signedTxtFiles = ["examples/dnssec/txt/ns.com."]

signedWildcardFiles = ["examples/dnssec/wildcard/ns.com."]

selfDelegFiles = ["examples/ns/self-deleg/ns.com."]

subSelfDelegFiles =
  [ "examples/ns/sub-self-deleg/root.net."
  , "examples/ns/sub-self-deleg/ns.net." ]

nsLoopFiles =
  [ "examples/ns/loop/ns.com."
  , "examples/ns/loop/root.net." ]

cnameLoopFiles = ["examples/cname/loop/ns.com."]

dnameLoopEmptyFiles = ["examples/dname/loop-empty/ns.net."]

dnameLoopNonEmptyFiles = ["examples/dname/loop-non-empty/ns.net."]

data Query = Query (Name String) RT.Type (Name String)
  deriving Eq

instance Show Query where
  show (Query zone rtype qname) =
    "query " ++ show rtype ++ " " ++ show qname ++ " from zone " ++ show zone

data Resolution = Resolution Query Term Term
  deriving Eq

instance Show Resolution where
  show (Resolution query verification auth) =
    show query ++ " with verified=" ++ show verification ++ " and auth=" ++ show auth

authenticated :: Name String -> RT.Type -> Name String -> Resolution
authenticated ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 1)

authenticatedUnsigned :: Name String -> RT.Type -> Name String -> Resolution
authenticatedUnsigned ns rtype qname =
  Resolution (Query ns rtype qname) top (ad 0)

dnsRoot, com, net, exampleZone, aCom, bCom, aaCom, subACom, aSubSubSubCom, exampleCom, xWildcardCom :: Name String
rootNet, subRootNet, nsCom, nsBCom, nsNet :: Name String
dnsRoot = root
com = mkName ["com", ""]
net = mkName ["net", ""]
exampleZone = mkName ["example", ""]
aCom = mkName ["a", "com", ""]
bCom = mkName ["b", "com", ""]
aaCom = mkName ["a", "a", "com", ""]
subACom = mkName ["sub", "a", "com", ""]
aSubSubSubCom = mkName ["a", "sub", "sub", "sub", "com", ""]
exampleCom = mkName ["example", "com", ""]
xWildcardCom = mkName ["x", "*", "com", ""]
rootNet = mkName ["root", "net", ""]
subRootNet = mkName ["sub", "root", "net", ""]
nsCom = mkName ["ns", "com", ""]
nsBCom = mkName ["ns", "b", "com", ""]
nsNet = mkName ["ns", "net", ""]

exampleFileTest :: ExampleFile -> TestTree
exampleFileTest example =
  testCase (examplePath example) $ do
    config <- loadZones [examplePath example]
    map zoneName config
      @?= map compileName (expectedZones example)

exampleConfigTest :: ExampleConfig -> TestTree
exampleConfigTest example =
  testCase (configName example) $ do
    results <- resolvableQueries (configFiles example)
    assertQueries
      (configFiles example)
      results
      (expectedQueries example)
      (unexpectedQueries example)

misconfigurationLoopTest :: [FilePath] -> [Query] -> [Query] -> Assertion
misconfigurationLoopTest files expected unexpected = do
  results <- resolvableQueries files
  assertQueries files results expected unexpected

exactResolutionTest :: [FilePath] -> [Term] -> Assertion
exactResolutionTest files expected = do
  results <- resolvableQueries files
  forM_ expected $ \resolution ->
    assertBool
      (show resolution ++ " was not resolvable for zones:\n" ++ showZoneFiles files)
      (any (coversTerm resolution) results)

authenticatedResolutionTest :: [FilePath] -> Assertion
authenticatedResolutionTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticated com RT.A aCom
    , authenticated com RT.A bCom
    , authenticated com RT.A nsBCom
    , authenticated com RT.CNAME aCom
    , authenticated com RT.DNSKEY com
    , authenticated com RT.DNSKEY bCom
    , authenticated com RT.DS bCom ]
    []

authenticatedWildcardResolutionTest :: [FilePath] -> Assertion
authenticatedWildcardResolutionTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticated com RT.A aCom
    , authenticated com RT.A bCom
    , authenticated com RT.A nsBCom
    , authenticated com RT.DNSKEY com ]
    []

authenticatedUnsignedDelegationTest :: [FilePath] -> Assertion
authenticatedUnsignedDelegationTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticatedUnsigned com RT.A bCom
    , authenticatedUnsigned com RT.A nsBCom ]
    [ authenticated com RT.A bCom
    , authenticated com RT.A nsBCom
    , authenticated com RT.DS bCom ]

signedDelegationDoesNotUseUnsignedProofTest :: [FilePath] -> Assertion
signedDelegationDoesNotUseUnsignedProofTest files = do
  results <- resolvableQueries files
  assertResolutions files results
    [ authenticated com RT.A bCom
    , authenticated com RT.A nsBCom
    , authenticated com RT.DS bCom ]
    [ authenticatedUnsigned com RT.A bCom
    , authenticatedUnsigned com RT.A nsBCom ]

doesNotAuthenticateUnsignedDelegationTest :: [FilePath] -> Assertion
doesNotAuthenticateUnsignedDelegationTest files = do
  results <- resolvableQueries files
  assertResolutions files results []
    [ authenticatedUnsigned com RT.A bCom
    , authenticatedUnsigned com RT.A nsBCom
    , authenticated com RT.A bCom
    , authenticated com RT.A nsBCom
    , authenticated com RT.DS bCom ]

regressionTest :: [FilePath] -> [Query] -> [Query] -> Assertion
regressionTest newConfig expected unexpected = do
  (oldRoots, oldKeys, oldZones) <- loadConfig baseFiles
  (newRoots, newKeys, newZones) <- loadConfig newConfig
  regressions <- failLeft $
    runFresh $
      runExceptT $
        do
          oldRules <- compileConfig oldRoots oldKeys oldZones
          newRules <- compileConfig newRoots newKeys newZones
          computeRegressions oldRules newRules
  assertResolvableQueries (baseFiles ++ newConfig) regressions expected unexpected

assertQueries :: [FilePath] -> S.Set Fact -> [Query] -> [Query] -> Assertion
assertQueries = assertQueriesWith queryTerm

assertResolvableQueries :: [FilePath] -> S.Set Fact -> [Query] -> [Query] -> Assertion
assertResolvableQueries = assertQueriesWith resolvableQueryTerm

assertQueriesWith :: (Query -> Term) -> [FilePath] -> S.Set Fact -> [Query] -> [Query] -> Assertion
assertQueriesWith toTerm files results expected unexpected = do
  forM_ expected $ \query ->
    assertBool
      (show query ++ " was not resolvable for zones:\n" ++ showZoneFiles files)
      (any (coversTerm (toTerm query)) results)
  forM_ unexpected $ \query ->
    assertBool
      (show query ++ " was wrongly resolvable for zones:\n" ++ showZoneFiles files)
      (not (any (coversTerm (toTerm query)) results))

assertResolutions :: [FilePath] -> S.Set Fact -> [Resolution] -> [Resolution] -> Assertion
assertResolutions files results expected unexpected = do
  forM_ expected $ \resolution ->
    assertBool
      (show resolution ++ " was not resolvable for zones:\n" ++ showZoneFiles files)
      (any (coversResolution resolution) results)
  forM_ unexpected $ \resolution ->
    assertBool
      (show resolution ++ " was wrongly resolvable for zones:\n" ++ showZoneFiles files)
      (not (any (coversResolution resolution) results))

showZoneFiles :: [FilePath] -> String
showZoneFiles = unlines . map ("  " ++)

coversResolution :: Resolution -> Fact -> Bool
coversResolution = coversTerm . resolutionTerm

coversTerm :: Term -> Fact -> Bool
coversTerm expected (Fact actual constraints) =
  isJust $ do
    u <- U.unify actual expected
    Store.solve $ Store.simplify $ U.apply u constraints

queryTerm :: Query -> Term
queryTerm (Query zone rtype qname) = runFresh $ do
  servers <- wildcard
  (wc1, wc2, wc3) <- wildcards3
  return $ resolves servers (ttypeTerm rtype) (nameTerm zone) (nameTerm qname) wc1 wc2 wc3

resolvableQueryTerm :: Query -> Term
resolvableQueryTerm (Query zone rtype qname) = runFresh $ resolvable (nameTerm zone) (ttypeTerm rtype) (nameTerm qname) <$> wildcard <*> wildcard

resolutionTerm :: Resolution -> Term
resolutionTerm (Resolution (Query zone rtype qname) verification auth) = runFresh $ do
  servers <- wildcard
  wc <- wildcard
  return $ resolves servers (ttypeTerm rtype) (nameTerm zone) (nameTerm qname) wc verification auth

resolvableQueries :: [FilePath] -> IO (S.Set Fact)
resolvableQueries files = do
  (roots, keys, zones) <- loadConfig files
  failLeft $ runFresh $ runExceptT $ do
    rules <- compileConfig roots keys zones
    lift $ immediateConsequences rules

loadZones :: [FilePath] -> IO [Zone ConstrainedLabel]
loadZones files = concatMap splitZone <$> parseZoneFiles (concatMap zoneWellformed . splitZone) files

loadConfig :: [FilePath] -> IO (Name String, [RRSet String], [Zone ConstrainedLabel])
loadConfig files = do
  cfg <- loadZones files
  roots <- case cfg of
    zone : _ -> return (toStringName $ zoneName zone)
    _ -> fail "configuration has no starting zones"
  keys <- concat <$> mapM loadKeys (nub $ map takeDirectory files)
  return (roots, keys, cfg)
  where
    loadKeys directory = do
      paths <- filter ("dsset-" `isPrefixOf`) . sort <$> listDirectory directory
      concat <$> mapM (parseDS . (directory </>)) paths
