module NameConstraintSpec (tests) where

import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import DNS.Name (Name (Name))
import NameConstraint (ConstrainedName, LabelConstraint(..), compileName, synthesize)

tests :: TestTree
tests =
  testGroup
    "NameConstraint"
    [ testCase "wildcard equality ignores constraints" $
        Wildcard ["a"] == Wildcard ["b"] @?= True
    , testCase "wildcard ordering ignores constraints" $
        compare (Wildcard ["a"]) (Wildcard ["b"]) @?= EQ
    , testGroup
        "synthesize (RFC 4592 section 2.2.1)"
        [ testCase "an equal name can answer the query" $
            let query = name ["host1", "example", ""]
            in synthesize query (compileName query) @?= True
        , testCase "host3.example is synthesized from *.example" $
            wildcardAnswers ["host3", "example", ""] @?= True
        , testCase "foo.bar.example is synthesized from *.example" $
            wildcardAnswers ["foo", "bar", "example", ""] @?= True
        , testCase "host1.example is blocked by the existing host1 node" $
            wildcardAnswers ["host1", "example", ""] @?= False
        , testCase "sub.*.example is blocked by the existing literal wildcard node" $
            wildcardAnswers ["sub", "*", "example", ""] @?= False
        , testCase "_telnet._tcp.host1.example is blocked by the existing host1 subtree" $
            wildcardAnswers ["_telnet", "_tcp", "host1", "example", ""] @?= False
        ]
    ]

-- At example., these are all the existing immediate children in the RFC's
-- complete zone.  They are the names that prevent *.example. from synthesis.
exampleWildcard :: ConstrainedName
exampleWildcard =
  Name $ NE.fromList
    [ Wildcard ["*", "host1", "host2", "subdel"]
    , Exactly "example"
    , Exactly ""
    ]

wildcardAnswers :: [String] -> Bool
wildcardAnswers labels = synthesize (name labels) exampleWildcard

name :: [String] -> Name String
name = Name . NE.fromList
