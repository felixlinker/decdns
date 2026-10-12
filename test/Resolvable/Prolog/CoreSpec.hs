module Resolvable.Prolog.CoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Prolog.Core

tests :: TestTree
tests =
  testGroup
    "Resolvable.Prolog.Core"
    [ testGroup
        "minNameLength"
        [ testCase "variable name" $
            minNameLength (TVar "Name") @?= 1
        , testCase "root name" $
            minNameLength (list []) @?= 1
        , testCase "single atom name" $
            minNameLength (list [TAtom "com"]) @?= 5
        , testCase "multiple atoms name" $
            minNameLength (list [TAtom "com", TAtom "example", TAtom "www"]) @?= 17
        , testCase "name containing variable label" $
            minNameLength (list [TAtom "com", TVar "Label"]) @?= 7
        , testCase "name with variable tail" $
            minNameLength (listWithTail [TVar "Label", TAtom "www"] (TVar "Suffix")) @?= 7
        ]
    ]
