module Resolvable.Prolog.CoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck

import Resolvable.Prolog.Core

tests :: TestTree
tests =
  testGroup
    "Resolvable.Prolog.Core"
    [ testProperty "rule equality and ordering ignore annotations" propRuleAnnotationIgnored
    , testGroup
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

genRule :: Gen (Rule Int)
genRule = (`fact` TAtom "head") <$> arbitrary

propRuleAnnotationIgnored :: Int -> Property
propRuleAnnotationIgnored annotation =
  forAll genRule $ \rule ->
    let updated = rule { rannotation = annotation }
    in conjoin
        [ (rule == updated) === True
        , compare rule updated === EQ
        ]
