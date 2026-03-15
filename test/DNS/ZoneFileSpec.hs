{- HLINT ignore "Use <$>" -}
module DNS.ZoneFileSpec
  ( tests
  , ArbZone(..)
  , genZone
  , genTXTRecord
  , genAbsoluteName
  ) where

import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import DNS.Name (Name (Name), Named (Named))
import DNS.Record
import DNS.ZoneFile
import Data.Maybe (fromMaybe)
import DNS.ZoneFile.Printing (printZone)

tests :: TestTree
tests =
  testGroup
    "DNS.ZoneFile"
    [ testProperty "getting data set on an empty zone returns that data" propSetGetRoundTrip
    , testProperty "pruning setting an empty array results in an empty tree" propPruningSetEmpty
    ]

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
  recGens <- listOf $ elements [ genNSRecord, genARecord, genAAAARecord, genTXTRecord, genMXRecord, genCNAMERecord, genDNAMERecord ]
  recs <- mapM ($ org) recGens
  return $ foldl insert (emptyZone org) (soa : recs)

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
