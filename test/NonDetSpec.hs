module NonDetSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck

import NonDet (NonDet)
import qualified NonDet

tests :: TestTree
tests =
  testGroup
    "NonDet"
    [ testProperty "choose exposes all given values" prop_chooseAll
    , testCase "fail is empty" $
        NonDet.all (NonDet.fail :: NonDet Int) @?= []
    , testCase "applicative combines choices" $
        NonDet.all ((+) <$> NonDet.choose [1, 2 :: Int] <*> NonDet.choose [10, 20 :: Int]) @?= [11, 21, 12, 22]
    ]

prop_chooseAll :: [Int] -> Property
prop_chooseAll xs = NonDet.all (NonDet.choose xs) === xs
