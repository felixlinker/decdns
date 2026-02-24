{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module DNS.NameSpec (tests) where

import Data.Char (toLower, toUpper)
import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DNS.Name hiding (name)

tests :: TestTree
tests =
  testGroup
    "ZoneFile"
    [ testProperty "isSubDomain is reflexive" propIsSubDomainReflexive
    , testProperty "isSubDomain holds for prefixed names" propIsSubDomainPrefix
    , testProperty "isSubDomain rejects different TLD" propIsSubDomainDiffTld
    , testProperty "canonicalCompare is case-insensitive" propCanonicalCompareCaseInsensitive
    , testProperty "canonicalCompare is antisymmetric" propCanonicalCompareAntisymmetric
    , testProperty "canonicalCompare orders by TLD first" propCanonicalCompareTldFirst
    , testGroup
        "unit"
        [ testCase "examples from RFC 4034" canonicalCompareRfc4034Example
        , testCase "more specific name sorts before parent" canonicalCompareSpecificBeforeParent
        , testCase "compares next label when TLD matches" canonicalCompareNextLabel
        ]
    ]

ordered :: [Name String]
ordered = map name
  [ [ "example" ]
  , [ "a", "example" ]
  , [ "yljkjljk", "a", "example" ]
  , [ "Z", "a", "example" ]
  , [ "zABC", "a", "EXAMPLE" ]
  , [ "z", "example" ]
  , [ "\001", "z", "example" ]
  , [ "*", "z", "example" ]
  , [ "\200", "z", "example" ] ]

canonicalCompareRfc4034Example :: Assertion
canonicalCompareRfc4034Example =
  let compared = zipWith canonicalCompare ordered (drop 1 ordered)
  in mapM_ (@?= LT) compared

canonicalCompareSpecificBeforeParent :: Assertion
canonicalCompareSpecificBeforeParent =
  canonicalCompare (name ["example", "com"]) (name ["com"]) @?= GT

canonicalCompareNextLabel :: Assertion
canonicalCompareNextLabel =
  canonicalCompare (name ["a", "example", "com"]) (name ["b", "example", "com"]) @?= LT

propIsSubDomainReflexive :: ArbName -> Property
propIsSubDomainReflexive (aname -> n) =
  isSubDomain n n === True

propIsSubDomainPrefix :: ArbName -> Property
propIsSubDomainPrefix (aname -> n) =
  forAll genLabels $ \prefix ->
    let n' = prefixName prefix n
    in isSubDomain n' n === True

propIsSubDomainDiffTld :: ArbName -> Property
propIsSubDomainDiffTld (aname -> n@(Name ls)) =
  forAll (genDifferentLabel (NE.last ls)) $ \otherTld ->
    let n' = Name (replaceLast ls otherTld)
    in isSubDomain n n' === False

propCanonicalCompareCaseInsensitive :: ArbName -> Property
propCanonicalCompareCaseInsensitive (aname -> n) =
  canonicalCompare n (fmap (map toUpper) n) === EQ

propCanonicalCompareAntisymmetric :: ArbName -> ArbName -> Property
propCanonicalCompareAntisymmetric (aname -> n1) (aname -> n2) =
  canonicalCompare n1 n2 === invert (canonicalCompare n2 n1)

propCanonicalCompareTldFirst :: Property
propCanonicalCompareTldFirst =
  forAll genLabel $ \tld1 ->
    forAll (genDifferentLabel tld1) $ \tld2 ->
      forAll genLabels $ \rest ->
        let n1 = name (rest ++ [tld1])
            n2 = name (rest ++ [tld2])
        in canonicalCompare n1 n2 === compare (map toLower tld1) (map toLower tld2)

newtype ArbName = ArbName (Name String)

instance Show ArbName where
  show = show . aname

aname :: ArbName -> Name String
aname (ArbName n) = n

instance Arbitrary ArbName where
  arbitrary = genName

genName :: Gen ArbName
genName = ArbName . name <$> listOf1 genLabel

genLabel :: Gen String
genLabel = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'])
  rest <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  return (first : rest)

genDifferentLabel :: String -> Gen String
genDifferentLabel s = suchThat genLabel (\t -> map toLower t /= map toLower s)

genLabels :: Gen [String]
genLabels = listOf genLabel

prefixName :: [String] -> Name String -> Name String
prefixName prefix (Name ls) = Name (NE.prependList prefix ls)

replaceLast :: NE.NonEmpty a -> a -> NE.NonEmpty a
replaceLast (_ NE.:| []) a = a NE.:| []
replaceLast (x NE.:| t) a = x NE.:| go t
  where
    go [] = []
    go [_] = [a]
    go (y:t') = y : go t'

name :: [String] -> Name String
name [] = error "name: empty label list"
name xs = Name (NE.fromList xs)

invert :: Ordering -> Ordering
invert LT = GT
invert GT = LT
invert EQ = EQ
