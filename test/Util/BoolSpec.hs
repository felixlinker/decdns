{- HLINT ignore "Evaluate" -}
module Util.BoolSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Util.Bool ((==>))

tests :: TestTree
tests =
  testGroup
    "Util.Bool"
    [ testCase "conjunction binds more tightly on the left" $
        (False && True ==> False) @?= True
    , testCase "conjunction binds more tightly on the right" $
        (False ==> False && False) @?= True
    ]
