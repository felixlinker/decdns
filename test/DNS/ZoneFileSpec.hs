{- HLINT ignore "Use <$>" -}
module DNS.ZoneFileSpec
  ( tests
  , ArbZone(..)
  , genZone
  , genTXTRecord
  , genAbsoluteName
  , normalizeZone
  ) where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import DNS.Name (Name (Name), Named (Named))
import DNS.Record
import qualified DNS.RecordType as RT
import DNS.SEC.Crypto
import DNS.ZoneFile
import Data.Maybe (fromMaybe)
import DNS.ZoneFile.Printing (printZone)
import Data.List (sort)

tests :: TestTree
tests =
  testGroup
    "DNS.ZoneFile"
    [ testProperty "getting data set on an empty zone returns that data" propSetGetRoundTrip
    , testProperty "pruning setting an empty array results in an empty tree" propPruningSetEmpty
    , testProperty "inserting two records is commutative" propInsertCommutative
    , testProperty "constructing a zone from its list of records preserves the zone" propAllDataFromListRoundtrip
    ]

normalizeZone :: Zone String -> [StandardRecord]
normalizeZone = sort . allData

propSetGetRoundTrip :: Property
propSetGetRoundTrip =
  forAll genAbsoluteName $ \org ->
    forAll (genSubDomain org) $ \n ->
      forAll (arbitrary :: Gen [Int]) $ \xs ->
        get n (set n xs (emptyZone org)) === Just xs

propPruningSetEmpty :: Property
propPruningSetEmpty =
  forAll genAbsoluteName $ \org ->
    forAll (genSubDomain org) $ \n ->
        isEmpty $ prune (set n [] (emptyZone org))

propInsertCommutative :: Property
propInsertCommutative =
  forAll genAbsRecord $ \r1 ->
    forAll genAbsRecord $ \r2 ->
      normalizeZone (mkZone r1 r2) === normalizeZone (mkZone r2 r1)
  where
    mkZone :: StandardRecord -> StandardRecord -> Zone String
    mkZone r1 = insert (singleton r1)

propAllDataFromListRoundtrip :: Property
propAllDataFromListRoundtrip =
  forAll genZone $ \z ->
    forAll (shuffle (allData z)) $ \rs ->
      maybe
      (counterexample "could not construct zone" (property False))
      ((normalizeZone z ===) . normalizeZone . fst)
      (L.uncons (fromRecordList rs))

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
  soa <- genSOARecord org
  recs <- listOf (genRecord org)
  return $ foldl insert (emptyZone org) (soa : recs)

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
  ]

genSOARecord :: Name String -> Gen StandardRecord
genSOARecord org = do
  ttl <- genTTL
  mname <- genAbsoluteName
  rname <- genAbsoluteName
  serial <- chooseInt (0, 2000000000)
  refresh <- chooseInt (0, 86400)
  retry <- chooseInt (0, 86400)
  expire <- chooseInt (0, 2000000000)
  minimumTtl <- chooseInt (0, 86400)
  pure $
    Named org (RData IN ttl (SOA mname rname serial refresh retry expire minimumTtl))

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
  return $ Named org (RData IN ttl (DNSKEY flags algorithm publicKey))

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
      (RData IN ttl (RRSIG typeCovered algorithm labelCount originalTTL signatureExpiration signatureInception keyTag signersName signature))

genNSECRecord :: Name String -> Gen StandardRecord
genNSECRecord org = do
  ownerName <- oneof [pure org, genSubDomain org]
  ttl <- genTTL
  nextDomainName <- genAbsoluteName
  typeBitMaps <- genTypeBitMaps
  return $ Named ownerName (RData IN ttl (NSEC nextDomainName typeBitMaps))

genDSRecord :: Name String -> Gen StandardRecord
genDSRecord org = do
  ownerName <- oneof [pure org, genSubDomain org]
  ttl <- genTTL
  keyTag <- chooseInt (0, 65535)
  algorithm <- genAlgorithm
  digestType <- genDigestType
  digest <- genBytes 1 64
  return $ Named ownerName (RData IN ttl (DS keyTag algorithm digestType digest))

genIPv4 :: Gen String
genIPv4 = do
  o1 <- chooseInt (0, 255)
  o2 <- chooseInt (0, 255)
  o3 <- chooseInt (0, 255)
  o4 <- chooseInt (0, 255)
  pure (show o1 ++ "." ++ show o2 ++ "." ++ show o3 ++ "." ++ show o4)

genIPv6Like :: Gen String
genIPv6Like = do
  groups <- vectorOf 8 genHexGroup
  pure (foldr1 (\a b -> a ++ ":" ++ b) groups)

genHexGroup :: Gen String
genHexGroup = do
  n <- chooseInt (1, 4)
  vectorOf n (elements (['0' .. '9'] ++ ['a' .. 'f']))

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
genRRSIGTime =
  oneof
    [ chooseInt (0, 2000000000)
    , elements [20240101000000, 20251231235959, 20300102030405]
    ]

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
  ]
