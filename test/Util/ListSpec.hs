module Util.ListSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck

import Util.List (zipStrict, zipWithStrict)

tests :: TestTree
tests =
  testGroup
    "Util.List"
    [ testProperty "zipWithStrict agrees with zipWith on equal lengths" prop_zipWithStrictMatchesZipWith
    , testProperty "zipStrict agrees with zip on equal lengths" prop_zipStrictMatchesZip
    , testCase "zipWithStrict rejects shorter right list" $
        zipWithStrict (+) [1, 2] [3 :: Int] @?= Nothing
    , testCase "zipWithStrict rejects shorter left list" $
        zipWithStrict (+) [1 :: Int] [2, 3] @?= Nothing
    , testCase "zipStrict works on equal lengths" $
        zipStrict [1, 2 :: Int] [3, 4 :: Int] @?= Just [(1, 3), (2, 4)]
    ]

prop_zipWithStrictMatchesZipWith :: [Int] -> [Int] -> Property
prop_zipWithStrictMatchesZipWith xs _ =
  forAll (vectorOf (length xs) (arbitrary :: Gen Int)) $ \zs ->
    zipWithStrict (+) xs zs === Just (zipWith (+) xs zs)

prop_zipStrictMatchesZip :: [Int] -> [Int] -> Property
prop_zipStrictMatchesZip xs _ =
  forAll (vectorOf (length xs) (arbitrary :: Gen Int)) $ \zs ->
    zipStrict xs zs === Just (zip xs zs)
