{- HLINT ignore "Use <$>" -}
module DNS.ZoneFileSpec
  ( tests
  , ArbZone(..)
  , genZone
  , genTXTRecord
  , genLabel
  , genName
  , genAbsoluteName
  , normalizeZone
  , named
  , zoneFromList
  ) where

import qualified Control.Monad.Trans.State as St
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Word (Word8, Word32)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified DNS.Name as N
import DNS.Name (Name (Name), Named (Named), (<|), Label)
import DNS.Record
import qualified DNS.RRSet as RRSet
import qualified DNS.RecordType as RT
import DNS.SEC.Crypto
import DNS.ZoneFile
import NameConstraint (ConstrainedLabel, ConstrainedName, LabelConstraint(..), compileWildcards)
import Data.Maybe (fromMaybe)
import DNS.ZoneFile.Printing (printZone)
import Data.List (sort)
import Data.IP (IPv4, toIPv4w, IPv6, toIPv6w)

tests :: TestTree
tests =
  testGroup
    "DNS.ZoneFile"
    [ testCase "getting a non-existent path returns Nothing" getEmptyIsNothing
    , testProperty "getting data set on an empty zone returns that data" propSetGetRoundTrip
    , testProperty "setting a wildcard path preserves the zone's wildcard constraints" propSetPreservesWildcardConstraints
    , testProperty "pruning setting an empty array results in an empty tree" propPruningSetEmpty
    , testProperty "inserting two records is commutative" propInsertCommutative
    , testCase "merging two zones keeps records from both zones" mergeZonesKeepsBothZones
    , testProperty "merging two zones is commutative" propMergeZonesCommutative
    , testProperty "constructing a zone from its list of records preserves the zone" propAllDataFromListRoundtrip
    , testProperty "path computation can be inverted" propPathComputation
    ]

normalizeZone :: Zone String -> [StandardRecord]
normalizeZone = sort . allRecords

named :: Name a -> RData -> Named a RRSet.RDataSet
named a = Named a . RRSet.singleton

zoneFromList :: (Label a, Ord a) => [Named a RRSet.RDataSet] -> IO (AZone a RRSet.RTypedSet RData)
zoneFromList = maybe (fail "could not create zone") (return . fromList) . NE.nonEmpty

getEmptyIsNothing :: Assertion
getEmptyIsNothing = get ("a" <| N.root) (emptyZone N.root :: Zone String) @?= Nothing

propSetGetRoundTrip :: Property
propSetGetRoundTrip =
  forAll genAbsoluteName $ \org ->
    forAll (genSubDomain org) $ \n ->
      forAll (arbitrary :: Gen [Int]) $ \xs ->
        get n (set n xs (emptyZone org)) === Just xs

type TestPath = NE.NonEmpty [String]

propSetPreservesWildcardConstraints :: Property
propSetPreservesWildcardConstraints =
  forAll genWildcardPath $ \path ->
    allPaths (set path RRSet.empty wildcardZone)
      === allPaths wildcardZone
  where
    genWildcardPath :: Gen ConstrainedName
    genWildcardPath = do
      Name ls <- elements [ n | Named n _ <- allData wildcardZone , any N.isWildcardLabel n ]
      Name <$> traverse varyConstraint ls

    varyConstraint :: ConstrainedLabel -> Gen ConstrainedLabel
    varyConstraint (Wildcard _) = Wildcard <$> listOf genLabel
    varyConstraint exact = pure exact

    allPaths :: Zone ConstrainedLabel -> [TestPath]
    allPaths z = St.execState (traverseNamed (\(Name ls) v -> St.modify (fmap constraint ls:) >> return v) z) []

    constraint :: ConstrainedLabel -> [String]
    constraint (Wildcard excluded) = excluded
    constraint (Exactly _) = []

    wildcardZone :: Zone ConstrainedLabel
    wildcardZone = compileWildcards $ fromList
      ( recordAt ("*" <| origin)
      NE.:| [ recordAt ("fixed" <| origin)
             , recordAt ("leaf" <| ("*" <| origin))
             , recordAt ("sibling" <| ("*" <| origin))
             , recordAt ("*" <| ("*" <| origin))
             ]
      )

    recordAt :: Name String -> Named String RRSet.RDataSet
    recordAt n = named n (RData IN 100 (TXT "wildcard"))

    origin :: Name String
    origin = "example" <| N.root

propPruningSetEmpty :: Property
propPruningSetEmpty =
  forAll genAbsoluteName $ \org ->
    forAll (listOf1 $ genSubDomain org) $ \ns ->
      isEmpty $ prune (foldr (`set` (RRSet.empty :: RRSet.RDataSet)) (emptyZone org) ns)

propInsertCommutative :: Property
propInsertCommutative =
  forAll genAbsRecord $ \r1 ->
    forAll genAbsRecord $ \r2 ->
      normalizeZone (mkZone r1 r2) === normalizeZone (mkZone r2 r1)
  where
    mkZone :: StandardRecord -> StandardRecord -> Zone String
    mkZone r1 = insert (singleton (fmap RRSet.singleton r1)) . fmap RRSet.singleton

propMergeZonesCommutative :: Property
propMergeZonesCommutative =
  forAll genZone $ \z1 ->
    forAll genZone $ \z2 ->
      normalizeZone (mergeZones z1 z2) === normalizeZone (mergeZones z2 z1)

mergeZonesKeepsBothZones :: Assertion
mergeZonesKeepsBothZones = do
  let parent = "b" <| N.root
      child = "a" <| parent
      parentData = RData IN 100 (TXT "parent")
      childData = RData IN 200 (TXT "child")
      parentRecord = Named parent parentData
      childRecord = Named child childData
  parentZone <- zoneFromList [named parent parentData]
  childZone <- zoneFromList [named child childData]
  normalizeZone (mergeZones parentZone childZone) @?= sort [parentRecord, childRecord]

propAllDataFromListRoundtrip :: Property
propAllDataFromListRoundtrip =
  forAll genZone $ \z ->
    forAll (shuffle (allRecords z)) $ \rs ->
      maybe
      (counterexample "could not construct zone" (property False))
      ((normalizeZone z ===) . normalizeZone . fromRecordList)
      (NE.nonEmpty rs)

propPathComputation :: Property
propPathComputation =
  forAll genAbsoluteName $ \n1 ->
    forAll genAbsoluteName $ \n2 ->
      let Path org left right = pathInZone n1 n2
      in n1 === foldl (flip (<|)) org left .&. n2 === foldl (flip (<|)) org right

data ArbZone = ArbZone
  { unArbZone :: Zone String
  , serialization :: String }

instance Show ArbZone where
  show = serialization

instance Arbitrary ArbZone where
  arbitrary = do
    z <- genZone
    abrev <- arbitrary :: Gen Bool
    let (origin NE.:| rs) = fromMaybe undefined $ NE.nonEmpty $ printZone abrev z
    rs' <- if abrev then return rs else shuffle rs
    return $ ArbZone z (unlines (origin:rs'))

genLabel :: Gen String
genLabel = elements ["a", "b", "c"]

genName :: Gen (Name String)
genName = do
  lbls <- listOf1 genLabel
  return (Name (NE.fromList lbls))

genAbsoluteName :: Gen (Name String)
genAbsoluteName = mkAbsolute <$> genName

genSubDomain :: Name String -> Gen (Name String)
genSubDomain (Name org) = do
  (Name n) <- genName
  return $ Name (n <> org)

mkAbsolute :: Name String -> Name String
mkAbsolute (Name ls) = Name (ls <> NE.singleton "")

-- Choose from few TTLs on purpose because we're primarily interested in whether
-- they are equal.
genTTL :: Gen Int
genTTL = elements [100, 200]

genZone :: Gen (Zone String)
genZone = do
  org <- genAbsoluteName
  apexData <- genApex org
  recs <- map (fmap RRSet.singleton) <$> listOf (genRecord org)
  return $ foldl insert (emptyZone org) (apexData : recs)

genAbsRecord :: Gen StandardRecord
genAbsRecord = genRecord =<< genAbsoluteName

genRecord :: Name String -> Gen StandardRecord
genRecord org = oneof $ map ($ org)
  [ genNSRecord
  , genARecord
  , genAAAARecord
  , genTXTRecord
  , genMXRecord
  , genCNAMERecord
  , genDNAMERecord
  , genDNSKEYRecord
  , genRRSIGRecord
  , genNSECRecord
  , genDSRecord
  , genZONEMDRecord
  ]

genApex :: Name String -> Gen (Named String RRSet.RDataSet)
genApex org = do
  ttl <- genTTL
  mname <- genAbsoluteName
  rname <- genAbsoluteName
  serial <- chooseInt (0, 2000000000)
  refresh <- chooseInt (0, 86400)
  retry <- chooseInt (0, 86400)
  expire <- chooseInt (0, 2000000000)
  minimumTtl <- chooseInt (0, 86400)
  let soa = RData IN ttl (SOA mname rname serial refresh retry expire minimumTtl)
  let ns =  RData IN ttl (NS mname)
  return $ Named org $ RRSet.fromList [soa, ns]

genNSRecord :: Name String -> Gen StandardRecord
genNSRecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  host <- genAbsoluteName
  return $ Named ownerName (RData IN ttl (NS host))

genARecord :: Name String -> Gen StandardRecord
genARecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  addr <- genIPv4
  return $ Named ownerName (RData IN ttl (A addr))

genAAAARecord :: Name String -> Gen StandardRecord
genAAAARecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  addr <- genIPv6Like
  return $ Named ownerName (RData IN ttl (AAAA addr))

genTXTRecord :: Name String -> Gen StandardRecord
genTXTRecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  txt <- genTxtAtom
  return $ Named ownerName (RData IN ttl (TXT txt))

genMXRecord :: Name String -> Gen StandardRecord
genMXRecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  pref <- chooseInt (0, 65535)
  exch <- genAbsoluteName
  return $ Named ownerName (RData IN ttl (MX pref exch))

genCNAMERecord :: Name String -> Gen StandardRecord
genCNAMERecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  target <- genAbsoluteName
  return $ Named ownerName (RData IN ttl (CNAME target))

genDNAMERecord :: Name String -> Gen StandardRecord
genDNAMERecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  target <- genAbsoluteName
  return $ Named ownerName (RData IN ttl (DNAME target))

genDNSKEYRecord :: Name String -> Gen StandardRecord
genDNSKEYRecord org = do
  ttl <- genTTL
  flags <- genDNSKeyFlags
  algorithm <- genAlgorithm
  publicKey <- genBytes 1 64
  return $ Named org (RData IN ttl (DNSKEY (DNSKEYData flags algorithm publicKey)))

genRRSIGRecord :: Name String -> Gen StandardRecord
genRRSIGRecord org = do
  ownerName <- oneof [pure org, genSubDomain org]
  ttl <- genTTL
  typeCovered <- elements nsecTypeUniverse
  algorithm <- genAlgorithm
  labelCount <- chooseInt (0, 10)
  originalTTL <- elements [ttl, 3600, 86400]
  signatureExpiration <- genRRSIGTime
  signatureInception <- genRRSIGTime
  keyTag <- chooseInt (0, 65535)
  signersName <- genAbsoluteName
  signature <- genBytes 1 96
  return $
    Named ownerName
      (RData IN ttl (RRSIG (RRSIGData typeCovered algorithm labelCount originalTTL signatureExpiration signatureInception keyTag signersName signature)))

genNSECRecord :: Name String -> Gen StandardRecord
genNSECRecord org = do
  ownerName <- oneof [pure org, genSubDomain org]
  ttl <- genTTL
  nextDomainName <- genAbsoluteName
  typeBitMaps <- genTypeBitMaps
  return $ Named ownerName (RData IN ttl (NSEC (NSECData nextDomainName typeBitMaps)))

genDSRecord :: Name String -> Gen StandardRecord
genDSRecord org = do
  ownerName <- oneof [pure org, genSubDomain org]
  ttl <- genTTL
  keyTag <- chooseInt (0, 65535)
  algorithm <- genAlgorithm
  digestType <- genDigestType
  digest <- genBytes 1 64
  return $ Named ownerName (RData IN ttl (DS (DSData keyTag algorithm digestType digest)))

genZONEMDRecord :: Name String -> Gen StandardRecord
genZONEMDRecord org = do
  ownerName <- oneof [pure org, genSubDomain org]
  ttl <- genTTL
  serial <- chooseInt (0, 2000000000)
  scheme <- chooseInt (0, 255)
  hashAlgorithm <- chooseInt (0, 255)
  digest <- genBytes 1 64
  return $ Named ownerName (RData IN ttl (ZONEMD serial scheme hashAlgorithm digest))

genIPv4 :: Gen IPv4
genIPv4 = toIPv4w <$> chooseAny

genIPv6Like :: Gen IPv6
genIPv6Like = toIPv6w <$> chooseAny

genTxtAtom :: Gen String
genTxtAtom =
  let chars = ['a' .. 'z'] ++ "-_./:"
  in (:) <$> elements chars <*> listOf (elements (' ' : chars))

genDNSKeyFlags :: Gen [DNSKeyFlag]
genDNSKeyFlags = sublistOf [ZoneKey, SecureEntryPoint, Other]

genAlgorithm :: Gen AlgorithmType
genAlgorithm =
  elements
    [ RSAMD5
    , DH
    , DSA
    , RSASHA1
    , DSA_NSEC3_SHA1
    , RSASHA1_NSEC3_SHA1
    , RSASHA256
    , RSASHA512
    , ECC_GOST
    , ECDSAP256SHA256
    , ECDSAP384SHA384
    , ED25519
    , ED448
    , SM2SM3
    , ECC_GOST12
    , INDIRECT
    , PRIVATEDNS
    , PRIVATEOID
    ]

genDigestType :: Gen DigestType
genDigestType = elements [NULL, SHA1, SHA256, GOST_R_31_11_94, SHA384]

genBytes :: Int -> Int -> Gen BS.ByteString
genBytes minLen maxLen = do
  len <- chooseInt (minLen, maxLen)
  BS.pack <$> vectorOf len genByte

genByte :: Gen Word8
genByte = fromIntegral <$> (chooseInt (0, 255) :: Gen Int)

genRRSIGTime :: Gen Int
genRRSIGTime = chooseInt (0, fromIntegral (maxBound :: Word32))

genTypeBitMaps :: Gen [RT.Type]
genTypeBitMaps = fmap L.nub (listOf1 (elements nsecTypeUniverse))

nsecTypeUniverse :: [RT.Type]
nsecTypeUniverse =
  [ RT.CNAME
  , RT.DNAME
  , RT.NS
  , RT.A
  , RT.AAAA
  , RT.TXT
  , RT.MX
  , RT.SOA
  , RT.DNSKEY
  , RT.RRSIG
  , RT.NSEC
  , RT.DS
  , RT.ZONEMD
  ]
