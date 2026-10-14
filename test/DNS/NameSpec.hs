{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module DNS.NameSpec
  ( tests
  , mkName ) where

import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DNS.Name hiding (name)
import DNS.ZoneFileSpec (genLabel, genName, genAbsoluteName)

tests :: TestTree
tests =
  testGroup
    "DNS.Name"
    [ testProperty "isSubDomain is reflexive" propIsSubDomainReflexive
    , testProperty "isSubDomain holds for prefixed names" propIsSubDomainPrefix
    , testProperty "isSubDomain rejects different TLD" propIsSubDomainDiffTld
    , testProperty "intersecting same names is identity" intersectIdentity
    , testProperty "intersection agrees with isSubDomain" intersectSubDomainRelation
    , testProperty "canonicalCompare is antisymmetric" propCanonicalCompareAntisymmetric
    , testProperty "canonicalCompare orders by TLD first" propCanonicalCompareTldFirst
    , testProperty "show absolute names with trailing dot" propShowAbsoluteNameEndsWithDot
    , testProperty "show relative names without trailing dot" propShowRelativeNameDoesNotEndWithDot
    , testGroup
        "unit"
        [ testCase "show root as dot" showRootAsDot
        , testCase "count labels in a wildcard name" $
            labelCount (mkName ["*", "example", ""]) @?= 1
        , testCase "count labels in a non-wildcard name" $
            labelCount (mkName ["example", ""]) @?= 1
        , testCase "count an intermediate wildcard label" $
            labelCount (mkName ["sub", "*", "example", ""]) @?= 3
        , testCase "examples from RFC 4034" canonicalCompareRfc4034Example
        , testCase "more specific name sorts before parent" canonicalCompareSpecificBeforeParent
        , testCase "compares next label when TLD matches" canonicalCompareNextLabel
        ]
    ]

showRootAsDot :: Assertion
showRootAsDot =
  show (root :: Name String) @?= "."

ordered :: [Name String]
ordered = map mkName
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
  canonicalCompare (mkName ["example", "com"]) (mkName ["com"]) @?= GT

canonicalCompareNextLabel :: Assertion
canonicalCompareNextLabel =
  canonicalCompare (mkName ["a", "example", "com"]) (mkName ["b", "example", "com"]) @?= LT

propIsSubDomainReflexive :: Property
propIsSubDomainReflexive =
  forAll genName $ \n -> counterexample (show n) (isSubDomain n n)

propIsSubDomainPrefix :: Property
propIsSubDomainPrefix =
  forAll genName $ \n ->
    forAll (listOf genLabel) $ \prefix ->
      isSubDomain (prependLabels prefix n) n

propIsSubDomainDiffTld :: Property
propIsSubDomainDiffTld =
  forAll genName $ \(Name ls) ->
    forAll genDifferentLabels $ \(l1, l2) ->
        not $ isSubDomain
          (Name (ls <> NE.singleton l1))
          (Name (ls <> NE.singleton l2))

propCanonicalCompareAntisymmetric :: Property
propCanonicalCompareAntisymmetric =
  forAll genName $ \n1 ->
    forAll genName $ \n2 ->
      canonicalCompare n1 n2 === invert (canonicalCompare n2 n1)

propCanonicalCompareTldFirst :: Property
propCanonicalCompareTldFirst =
  forAll genDifferentLabels $ \(tld1, tld2) ->
    forAll genName $ \(Name ls) ->
      let n1 = Name (ls <> NE.singleton tld1)
          n2 = Name (ls <> NE.singleton tld2)
      in canonicalCompare n1 n2 === compare (map toLower tld1) (map toLower tld2)

propShowAbsoluteNameEndsWithDot :: Property
propShowAbsoluteNameEndsWithDot =
  forAll genAbsoluteName $ \(show -> n) -> counterexample n (last n === '.')

propShowRelativeNameDoesNotEndWithDot :: Property
propShowRelativeNameDoesNotEndWithDot =
  forAll genName $ \(show -> n) -> counterexample n (last n =/= '.')

intersectIdentity :: Property
intersectIdentity = forAll genName $ \n -> intersection n n === Just n

intersectSubDomainRelation :: Property
intersectSubDomainRelation =
  forAll genName $ \n1@(Name (NE.last -> last1)) ->
    forAll genName $ \n2@(Name (NE.last -> last2)) ->
      let maybeCommonPrefix = intersection n1 n2
      in  maybe (last1 =/= last2)
                (\pref -> counterexample (showCex n1 n2 pref) $ isSubDomain n1 pref && isSubDomain n2 pref)
                maybeCommonPrefix
  where
    showCex :: Name String -> Name String -> Name String -> String
    showCex n1 n2 pref = unwords [show n1, "/\\", show n2, "=", show pref]

genDifferentLabels :: Gen (String, String)
genDifferentLabels = do
  l1 <- arbitrary :: Gen Char
  l2 <- suchThat arbitrary (/= l1)
  return (l1:"", l2:"")

mkName :: [String] -> Name String
mkName = Name . NE.fromList

invert :: Ordering -> Ordering
invert LT = GT
invert GT = LT
invert EQ = EQ
