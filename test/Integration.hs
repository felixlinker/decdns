module Integration (tests) where

import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Set as S
import System.FilePath ((</>))

import DNS.Name (Name, root)
import DNS.NameSpec (mkName)
import DNS.RRSet (RRSet)
import qualified DNS.RecordType as RT
import DNS.Wellformedness (zoneWellformed)
import DNS.Zone (splitZone)
import DNS.ZoneFile (Zone)
import NameConstraint (ConstrainedLabel)
import Parsing (parseZoneFiles, parseDS)
import Resolvable.Consequences
import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving (getSolved)
import Resolvable.Prolog.Core
import Resolvable.Resolvability (compileConfig)
import Resolvable.Update.Dependencies (computeRegressions)
import qualified Resolvable.Unification as U
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Util.Fresh (runFresh, runFreshT)
import Util.Monad (failLeft)
import Data.Maybe (fromMaybe)
import Resolvable.Unification (away)

wc1, wc2 :: Term
wc1 = TVar "Wildcard1"
wc2 = TVar "Wildcard2"

tests :: TestTree
tests = testGroup "Examples"
  [ testGroup "resolvability" $ map exampleConfigTest exampleConfigs
  , testCase "signed facts have fully reduced constraints" $ do
      results <- resolvableQueries cnameSigned
      forM_ results $ \(Fact _ constraints) ->
        fmap Store.toBody (getSolved (Store.simplify constraints))
          @?= Just (Store.toBody constraints)
  , testGroup "Resolvable.Update.Dependencies"
      [ testCase "base to new has expected unsigned regression facts" $
          regressionTest cnameNew []
            [ resolvable (nameTerm com) (ttypeTerm RT.A) (nameTerm aCom) wc1 wc2
            , resolvable (nameTerm com) (ttypeTerm RT.A) (nameTerm bCom) wc1 wc2
            , resolvable (nameTerm com) (ttypeTerm RT.A) (nameTerm nsBCom) wc1 wc2 ]
      , testCase "base to broken has expected unsigned regression facts" $
          regressionTest cnameBroken
            [ resolvable (nameTerm com) (ttypeTerm RT.A) (nameTerm aCom) wc1 wc2
            , resolvable (nameTerm com) (ttypeTerm RT.A) (nameTerm bCom) wc1 wc2
            , resolvable (nameTerm com) (ttypeTerm RT.A) (nameTerm nsBCom) wc1 wc2 ] []
      ]
  ]

-- Paths are relative to examples/configName; an absolute DS path is also allowed.
-- The root zone identifies the NS RRSet used as starting servers.
data ExampleConfig = ExampleConfig
  { configName :: FilePath
  , configRootDS :: FilePath
  , configRootZone :: Name String
  , configFiles :: [FilePath]
  , expectedQueries :: [Term]
  , unexpectedQueries :: [Term]
  }

exampleConfigs :: [ExampleConfig]
exampleConfigs =
  [ cnameBase
  , cnameNew
  , cnameBroken
  , cnamePartiallySigned
  , cnameSigned
  , cnameSignedNonZoneKey
  , cnamePartiallySignedNsec3
  , cnamePartiallySignedNsec3Oo
  , cnameSignedNsec3
  , dnssecNs
  , nsSelfDeleg
  , nsSubSelfDeleg
  , dnameLoopNonEmpty
  , cnameSignedNew
  , dnssecTxt
  , dnssecWildcard
  , nsLoop
  , cnameLoop
  , dnameLoopEmpty
  , dnameMappedDelegation
  , leafUnsigned
  , dnameSequence
  ]

wc :: Maybe a
wc = Nothing

gr :: a -> Maybe a
gr = Just

validate :: Maybe Term
validate = Just top

noValidate :: Maybe Term
noValidate = Just bot

ad0 :: Maybe Term
ad0 = Just $ ad 0

ad1 :: Maybe Term
ad1 = Just $ ad 1

cnameBase :: ExampleConfig
cnameBase =
  ExampleConfig
    "cname/base"
    "/dev/null"
    com
    ["ns.com.", "ns.b.com."]
    [ query wc wc wc wc wc
    , query wc (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) wc (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) wc noValidate ad0
    , query (gr com) (gr RT.A) (gr aCom) wc ad0
    , query (gr com) (gr RT.A) (gr aCom) noValidate wc
    , query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr bCom) noValidate ad0
    , query (gr bCom) (gr RT.A) (gr bCom) noValidate ad0 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr aCom) validate ad1
    , query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.CNAME) (gr aCom) validate ad1
    , query (gr com) (gr RT.NS) (gr bCom) validate ad1
    , query (gr bCom) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.NS) (gr com) validate ad1
    , query (gr bCom) (gr RT.NS) (gr bCom) validate ad1 ]

cnameNew :: ExampleConfig
cnameNew =
  ExampleConfig
    "cname/new"
    "/dev/null"
    com
    ["ns.com."]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr nsBCom) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr com) noValidate ad0 ]
    [ query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr com) validate ad1 ]

cnameBroken :: ExampleConfig
cnameBroken =
  ExampleConfig
    "cname/broken"
    "/dev/null"
    com
    ["ns.com."]
    [ query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr com) noValidate ad0 ]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr com) validate ad1 ]

cnamePartiallySigned :: ExampleConfig
cnamePartiallySigned =
  ExampleConfig
    "cname/partially-signed"
    "dsset-com."
    com
    ["ns.com.", "ns.b.com."]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr com) noValidate ad0
    , query (gr com) (gr RT.NSEC) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad0
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad0 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad1
    , query (gr com) (gr RT.DS) (gr bCom) validate ad1
    , query (gr bCom) (gr RT.NS) (gr bCom) validate ad1 ]

cnameSigned :: ExampleConfig
cnameSigned =
  ExampleConfig
    "cname/signed"
    "dsset-com."
    com
    ["ns.com.", "ns.b.com."]
    [ query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad1
    , query (gr com) (gr RT.DS) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr aCom) validate ad1
    , query (gr com) (gr RT.CNAME) (gr aCom) validate ad1
    , query (gr com) (gr RT.DNSKEY) (gr com) validate ad1
    , query (gr com) (gr RT.DNSKEY) (gr bCom) validate ad1 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad0
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad0 ]

cnameSignedNonZoneKey :: ExampleConfig
cnameSignedNonZoneKey =
  ExampleConfig
    "cname/signed-non-zone-key"
    "dsset-com."
    com
    ["ns.com.", "ns.b.com."]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr com) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.DS) (gr bCom) noValidate ad0 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0 ]

cnamePartiallySignedNsec3 :: ExampleConfig
cnamePartiallySignedNsec3 =
  ExampleConfig
    "cname/partially-signed-nsec3"
    "dsset-com."
    com
    ["ns.com.", "ns.b.com."]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr com) noValidate ad0
    , query (gr com) (gr RT.NSEC3PARAM) (gr com) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad0
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad0 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad1
    , query (gr com) (gr RT.DS) (gr bCom) validate ad1
    , query (gr bCom) (gr RT.NS) (gr bCom) validate ad1 ]

cnamePartiallySignedNsec3Oo :: ExampleConfig
cnamePartiallySignedNsec3Oo =
  ExampleConfig
    "cname/partially-signed-nsec3-oo"
    "dsset-com."
    com
    ["ns.com.", "ns.b.com."]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr com) noValidate ad0
    , query (gr com) (gr RT.NSEC3PARAM) (gr com) noValidate ad0 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad0
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad1
    , query (gr com) (gr RT.DS) (gr bCom) validate ad1
    , query (gr bCom) (gr RT.NS) (gr bCom) validate ad1 ]

cnameSignedNsec3 :: ExampleConfig
cnameSignedNsec3 =
  ExampleConfig
    "cname/signed-nsec3"
    "dsset-com."
    com
    ["ns.com.", "ns.b.com."]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr com) noValidate ad0
    , query (gr com) (gr RT.DNSKEY) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.DS) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NSEC3PARAM) (gr com) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad1
    , query (gr com) (gr RT.DS) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr aCom) validate ad1
    , query (gr com) (gr RT.CNAME) (gr aCom) validate ad1
    , query (gr com) (gr RT.DNSKEY) (gr com) validate ad1
    , query (gr com) (gr RT.DNSKEY) (gr bCom) validate ad1 ]
    [ query (gr com) (gr RT.TXT) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) validate ad0
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad0 ]

dnssecNs :: ExampleConfig
dnssecNs =
  ExampleConfig
    "dnssec/ns"
    "dsset-example."
    exampleZone
    ["example."]
    [ query (gr exampleZone) (gr RT.NS) (gr exampleZone) noValidate ad0
    , query (gr exampleZone) (gr RT.NS) (gr exampleZone) validate ad1 ]
    []

nsSelfDeleg :: ExampleConfig
nsSelfDeleg =
  ExampleConfig
    "ns/self-deleg"
    "/dev/null"
    com
    ["ns.com."]
    [ query (gr com) (gr RT.NS) (gr com) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr com) noValidate ad0
    , query (gr com) (gr RT.A) (gr nsCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr subACom) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr subACom) noValidate ad0
    , query (gr com) (gr RT.A) (gr subACom) noValidate ad0 ]
    [ query (gr com) (gr RT.TXT) (gr subACom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr com) validate ad1
    , query (gr subACom) (gr RT.NS) (gr subACom) validate ad1 ]

nsSubSelfDeleg :: ExampleConfig
nsSubSelfDeleg =
  ExampleConfig
    "ns/sub-self-deleg"
    "/dev/null"
    dnsRoot
    ["root.net.", "ns.net."]
    [ query (gr dnsRoot) (gr RT.NS) (gr dnsRoot) noValidate ad0
    , query (gr dnsRoot) (gr RT.SOA) (gr dnsRoot) noValidate ad0
    , query (gr dnsRoot) (gr RT.NS) (gr net) noValidate ad0
    , query (gr dnsRoot) (gr RT.A) (gr nsNet) noValidate ad0
    , query (gr net) (gr RT.NS) (gr net) noValidate ad0
    , query (gr net) (gr RT.SOA) (gr net) noValidate ad0
    , query (gr net) (gr RT.NS) (gr rootNet) noValidate ad0
    , query (gr net) (gr RT.A) (gr rootNet) noValidate ad0
    , query (gr dnsRoot) (gr RT.NS) (gr rootNet) noValidate ad0
    , query (gr dnsRoot) (gr RT.SOA) (gr rootNet) noValidate ad0
    , query (gr dnsRoot) (gr RT.A) (gr rootNet) noValidate ad0
    , query (gr net) (gr RT.A) (gr subRootNet) noValidate ad0 ]
    [ query (gr dnsRoot) (gr RT.TXT) (gr subRootNet) noValidate ad0
    , query (gr dnsRoot) (gr RT.NS) (gr dnsRoot) validate ad1
    , query (gr net) (gr RT.NS) (gr net) validate ad1
    , query (gr rootNet) (gr RT.NS) (gr rootNet) validate ad1 ]

dnameLoopNonEmpty :: ExampleConfig
dnameLoopNonEmpty =
  ExampleConfig
    "dname/loop-non-empty"
    "/dev/null"
    com
    ["ns.net."]
    [ query (gr com) (gr RT.TXT) (gr aSubSubSubCom) noValidate ad0 ]
    [ query (gr com) (gr RT.NS) (gr com) validate ad1 ]

cnameSignedNew :: ExampleConfig
cnameSignedNew =
  ExampleConfig
    "cname/signed-new" "dsset-com." com ["ns.com."]
    [ query (gr com) (gr RT.A) (gr aCom) validate ad1
    , query (gr com) (gr RT.A) (gr bCom) validate ad1
    , query (gr com) (gr RT.A) (gr nsBCom) validate ad1
    , query (gr com) (gr RT.DNSKEY) (gr com) validate ad1 ]
    -- The wildcard cannot synthesize an answer at an existing owner.
    [ query (gr com) (gr RT.A) (gr com) noValidate ad0 ]

dnssecTxt :: ExampleConfig
dnssecTxt =
  ExampleConfig
    "dnssec/txt" "dsset-com." com ["ns.com."]
    [ query (gr com) (gr RT.TXT) (gr com) validate ad1 ]
    []

dnssecWildcard :: ExampleConfig
dnssecWildcard =
  ExampleConfig
    "dnssec/wildcard" "dsset-com." com ["ns.com."]
    [ query (gr com) (gr RT.A) (gr aCom) validate ad1
    , query (gr com) (gr RT.A) (gr xWildcardCom) validate ad1 ]
    [ query (gr com) (gr RT.A) (gr com) noValidate ad0 ]

nsLoop :: ExampleConfig
nsLoop =
  ExampleConfig
    "ns/loop" "/dev/null" dnsRoot ["ns.com.", "root.net."]
    []
    [ query (gr dnsRoot) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr dnsRoot) (gr RT.NS) (gr dnsRoot) validate ad1 ]

cnameLoop :: ExampleConfig
cnameLoop =
  ExampleConfig
    "cname/loop" "/dev/null" com ["ns.com."]
    [ query (gr com) (gr RT.CNAME) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.CNAME) (gr bCom) noValidate ad0]
    [ query (gr com) (gr RT.A) (gr aCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr com) validate ad1 ]

dnameLoopEmpty :: ExampleConfig
dnameLoopEmpty =
  ExampleConfig
    "dname/loop-empty" "/dev/null" com ["ns.net."]
    [ query (gr com) (gr RT.DNAME) (gr aaCom) noValidate ad0
    , query (gr com) (gr RT.DNAME) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr com) noValidate ad0
    , query (gr com) (gr RT.SOA) (gr com) noValidate ad0 ]
    [ query (gr com) (gr RT.A) (gr aaCom) noValidate ad0
    , query (gr com) (gr RT.A) (gr bCom) noValidate ad0
    , query (gr com) (gr RT.NS) (gr com) validate ad1 ]

dnameMappedDelegation :: ExampleConfig
dnameMappedDelegation =
  ExampleConfig "dname/mapped-delegation" "/dev/null" fnniFnniNet ["zone"]
    [ query wc (gr RT.SOA) (gr fnniFnniFnniFnniNet) wc wc ]
    []

leafUnsigned :: ExampleConfig
leafUnsigned =
  ExampleConfig "ns/leaf-unsigned" "dsset-root" dnsRoot ["root", "example", "sub.example"]
    [ query wc (gr RT.TXT) (gr subExample) validate ad0 ]
    [ query wc (gr RT.TXT) (gr subExample) validate ad1 ]

dnameSequence :: ExampleConfig
dnameSequence =
  ExampleConfig "dname/sequence" "/dev/null" dnsRoot ["zone"]
    [ query wc (gr RT.DNAME) (gr labelA) wc wc
    , query wc (gr RT.TXT) (gr xLabelA) wc wc ]
    []

dnsRoot, com, net, exampleZone, aCom, bCom, aaCom, subACom, aSubSubSubCom, xWildcardCom :: Name String
rootNet, subRootNet, nsCom, nsBCom, nsNet, fnniFnniNet, fnniFnniFnniFnniNet, subExample :: Name String
labelA, xLabelA :: Name String
dnsRoot = root
com = mkName ["com", ""]
net = mkName ["net", ""]
exampleZone = mkName ["example", ""]
aCom = mkName ["a", "com", ""]
bCom = mkName ["b", "com", ""]
aaCom = mkName ["a", "a", "com", ""]
subACom = mkName ["sub", "a", "com", ""]
aSubSubSubCom = mkName ["a", "sub", "sub", "sub", "com", ""]
xWildcardCom = mkName ["x", "*", "com", ""]
rootNet = mkName ["root", "net", ""]
subRootNet = mkName ["sub", "root", "net", ""]
nsCom = mkName ["ns", "com", ""]
nsBCom = mkName ["ns", "b", "com", ""]
nsNet = mkName ["ns", "net", ""]
fnniFnniNet = mkName ["fnni", "fnni", "net", ""]
fnniFnniFnniFnniNet = mkName ["fnni", "fnni", "fnni", "fnni", "net", ""]
subExample = mkName ["sub", "example", ""]
labelA = mkName ["_label", "a", ""]
xLabelA = mkName ["x", "_label", "a", ""]

exampleConfigTest :: ExampleConfig -> TestTree
exampleConfigTest config = withResource (resolvableQueries config) (const $ return ()) $ \consequences ->
  testGroup (configName config)
    [ testCase ((if expected then "has solution: " else "has no solution: ") ++ show term)
        (consequences >>= assertMatch expected term)
    | (expected, terms) <- [(True, expectedQueries config), (False, unexpectedQueries config)]
    , term <- terms
    ]

assertMatch :: Bool -> Term -> S.Set Fact -> Assertion
assertMatch expected term facts =
  assertBool (show term ++ if expected then " had no matching fact" else " unexpectedly matched a fact")
    (any (coversTerm term) facts == expected)

coversTerm :: Term -> Fact -> Bool
coversTerm t f = maybe False fst $ (`runFreshT` 0) $ do
  (expected, _) <- away t
  Fact actual constraints <- awayF f
  u <- lift $ U.unify actual expected
  return $ fromMaybe False $ Store.solve (Store.simplify (U.apply u constraints))

query :: Maybe (Name String) -> Maybe RT.Type -> Maybe (Name String) -> Maybe Term -> Maybe Term -> Term
query mZone mRtype mQname mVerification mAuth = runFresh $ do
  servers <- wildcard
  zone <- maybe wildcard (return . nameTerm) mZone
  rtype <- maybe wildcard (return . ttypeTerm) mRtype
  qname <- maybe wildcard (return . nameTerm) mQname
  dat <- wildcard
  ver <- maybe wildcard return mVerification
  auth <- maybe wildcard return mAuth
  return $ resolves servers rtype zone qname dat ver auth

resolvableQueries :: ExampleConfig -> IO (S.Set Fact)
resolvableQueries config = do
  (roots, keys, zones) <- loadConfig config
  failLeft $ runFresh $ runExceptT $ do
    rules <- compileConfig roots keys zones
    lift $ immediateConsequences rules

loadZones :: [FilePath] -> IO [Zone ConstrainedLabel]
loadZones files = concatMap splitZone <$> parseZoneFiles (concatMap zoneWellformed . splitZone) files

loadConfig :: ExampleConfig -> IO (Name String, [RRSet String], [Zone ConstrainedLabel])
loadConfig config = do
  zones <- loadZones (map (directory </>) (configFiles config))
  keys <- parseDS (directory </> configRootDS config)
  return (configRootZone config, keys, zones)
  where
    directory = "examples" </> configName config

regressionTest :: ExampleConfig -> [Term] -> [Term] -> Assertion
regressionTest newConfig expected unexpected = do
  (oldRoots, oldKeys, oldZones) <- loadConfig cnameBase
  (newRoots, newKeys, newZones) <- loadConfig newConfig
  regressions <- failLeft $ runFresh $ runExceptT $ do
    oldRules <- compileConfig oldRoots oldKeys oldZones
    newRules <- compileConfig newRoots newKeys newZones
    computeRegressions oldRules newRules
  forM_ expected $ \term -> assertMatch True term regressions
  forM_ unexpected $ \term -> assertMatch False term regressions
