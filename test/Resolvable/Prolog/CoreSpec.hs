module Resolvable.Prolog.CoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Resolvable.Prolog.Core
import Util.Fresh (runFresh)
import Resolvable.Unification (equivModuloRenaming)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Prolog.Core"
    [ testCase "away preserves variable sharing" $
        let t = TFunctor "pair" [TVar "X", TVar "X", TVar "Y"]
        in  equivModuloRenaming (runFresh $ away t) t @?= True
    , testGroup
        "minNameLength"
        [ testCase "variable name" $
            minNameLength (TVar "Name") @?= 2
        , testCase "single atom name" $
            minNameLength (list [TAtom "com"]) @?= 6
        , testCase "multiple atoms name" $
            minNameLength (list [TAtom "www", TAtom "example", TAtom "com"]) @?= 18
        , testCase "name containing variable label" $
            minNameLength (list [TVar "Label", TAtom "com"]) @?= 8
        , testCase "name with variable tail" $
            minNameLength (listWithTail [TAtom "www", TVar "Label"] (TVar "Suffix")) @?= 8
        ]
    ]
