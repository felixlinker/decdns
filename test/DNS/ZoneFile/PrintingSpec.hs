{- HLINT ignore "Use <$>" -}
module DNS.ZoneFile.PrintingSpec (tests) where

import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity (runIdentity)
import Data.List (sort)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import DNS.Name (Name (Name), Named (Named))
import DNS.Record
  ( Cat (IN)
  , RData (RData)
  , StandardRecord
  , Type (A, AAAA, CNAME, DNAME, MX, NS, SOA, TXT)
  )
import DNS.ZoneFile (Zone, allData, emptyZone, insert, zoneName)
import DNS.ZoneFile.Printing (printZone)
import Parsing.ZoneFile (parseZoneFile)
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
  testGroup
    "ZoneFile.Printing"
    [ testProperty "parsing a printed zone yields the same zone" propPrintParseRoundTrip
    ]

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

propPrintParseRoundTrip :: ArbZone -> Property
propPrintParseRoundTrip (ArbZone z printed) =
  let parsed = runIdentity (parseZoneFile "<quickcheck>" printed)
  in case parsed of
      Left err ->
        counterexample (show err) False
      Right [z'] ->
        normalizeZone z' === normalizeZone z
      Right zs ->
        counterexample ("expected 1 zone, got " ++ show (length zs)) False

normalizeZone :: Zone String -> (Name String, [StandardRecord])
normalizeZone z = (zoneName z, sort (allData z))

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
  let z = foldl insert (emptyZone org) (soa : recs)
  return z

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
  return $
    Named org (RData IN ttl (SOA mname rname serial refresh retry expire minimumTtl))

genNSRecord :: Name String -> Gen StandardRecord
genNSRecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  host <- genAbsoluteName
  return (Named ownerName (RData IN ttl (NS host)))

genARecord :: Name String -> Gen StandardRecord
genARecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  addr <- genIPv4
  return (Named ownerName (RData IN ttl (A addr)))

genAAAARecord :: Name String -> Gen StandardRecord
genAAAARecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  addr <- genIPv6Like
  return (Named ownerName (RData IN ttl (AAAA addr)))

genTXTRecord :: Name String -> Gen StandardRecord
genTXTRecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  txt <- genTxtAtom
  return (Named ownerName (RData IN ttl (TXT txt)))

genMXRecord :: Name String -> Gen StandardRecord
genMXRecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  pref <- chooseInt (0, 65535)
  exch <- genAbsoluteName
  return (Named ownerName (RData IN ttl (MX pref exch)))

genCNAMERecord :: Name String -> Gen StandardRecord
genCNAMERecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  target <- genAbsoluteName
  return (Named ownerName (RData IN ttl (CNAME target)))

genDNAMERecord :: Name String -> Gen StandardRecord
genDNAMERecord org = do
  ownerName <- genSubDomain org
  ttl <- genTTL
  target <- genAbsoluteName
  return (Named ownerName (RData IN ttl (DNAME target)))

genLabel :: Gen String
genLabel = elements ["a", "b", "c"]

genIPv4 :: Gen String
genIPv4 = do
  o1 <- chooseInt (0, 255)
  o2 <- chooseInt (0, 255)
  o3 <- chooseInt (0, 255)
  o4 <- chooseInt (0, 255)
  return (show o1 ++ "." ++ show o2 ++ "." ++ show o3 ++ "." ++ show o4)

genIPv6Like :: Gen String
genIPv6Like = do
  groups <- vectorOf 8 genHexGroup
  return (foldr1 (\a b -> a ++ ":" ++ b) groups)

genHexGroup :: Gen String
genHexGroup = do
  n <- chooseInt (1, 4)
  vectorOf n (elements (['0' .. '9'] ++ ['a' .. 'f']))

genTxtAtom :: Gen String
genTxtAtom =
  let chars = ['a' .. 'z'] ++ "-_./:"
  in (:) <$> elements chars <*> listOf (elements (' ':chars))
