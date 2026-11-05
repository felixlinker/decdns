module Resolvable.Constraint.NeqsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Resolvable.Prolog.Core (Term(..), list, listWithTail)
import Resolvable.Constraint.Neqs

tests :: TestTree
tests =
  testGroup
    "Resolvable.Constraint.Neqs"
    [ testGroup
        "unit"
        ([ testCase "simplify rejects equal terms" simplifyRejectsEqualGroundTerms
        , testCase "simplify identity" simplifyKeepsVariableConstraints ]
        ++ map (\(n, i, ex) -> simplifyFunctorCase n i ex) functorSimplifications
        )
    ]

simplifyRejectsEqualGroundTerms :: Assertion
simplifyRejectsEqualGroundTerms =
  simplify [(TAtom "a", TAtom "a")] @?= []

simplifyKeepsVariableConstraints :: Assertion
simplifyKeepsVariableConstraints =
        simplify  [(TVar "X", TVar "Y"), (TAtom "a", TVar "Z")]
    @?= [[(TVar "X", TVar "Y"), (TAtom "a", TVar "Z")]]

(!=) :: Term -> Term -> (Term, Term)
(!=) = (,)

f :: Term -> Term -> Term
f i j = TFunctor "f" [i, j]

g :: Term -> Term -> Term
g i j = TFunctor "g" [i, j]

h :: Term -> Term -> Term -> Term
h i j k = TFunctor "h" [i, j, k]

x, y, z, a, b :: Term
x = TVar "X"
y = TVar "Y"
z = TVar "Z"
a = TAtom "a"
b = TAtom "b"

functorSimplifications :: [(String, (Term, Term), [[(Term, Term)]])]
functorSimplifications =
  [ ("a", f x b != f a b, [[x != a]])
  , ("b", f a b != f a b, [])
  , ("c", g a b != f a b, [[]])
  , ("d", f (g x b) b != f (g y a) b, [[]])
  , ("e", f (g x a) a != f (g y a) a, [[x != y]])
  , ("f", f (g x (f y b)) a != f (g y (f z b)) a, [[x != y], [y != z]])
  , ("g", f a (f a (f a (f a x))) != f a (f a (f a (f a y))), [[x != y]])
  , ("h", listWithTail [x] y != list [a, b], [[x != a], [y != list [b]]])
  , ("i", h x y (h z a b) != h z x (h y a b), [[x != z], [y != x], [z != y]])
  , ("j", list [x, y, z] != list [z, x, y], [[x != z], [y != x], [z != y]])
  ]

simplifyFunctorCase :: String -> (Term, Term) -> [[(Term, Term)]] -> TestTree
simplifyFunctorCase name neq expected =
  testCase name (simplify [neq] @?= expected)
