{-# LANGUAGE ViewPatterns #-}
module Resolvable.Prolog
  ( Term(..)
  , Rule(..)
  , plWildcard
  , dnsWildcard
  , typeAtom
  , resolvable
  , neq
  , nameWithSynthesis
  , nameTerm ) where

import NameConstraint
import ZoneFile

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, toUpper)
import Util.Fresh (Fresh, fresh)

data Term = TVar String | TAtom String | TList [Term] (Maybe Term) | TPred String [Term]

instance Show Term where
  show (TVar v) = map toUpper v
  show (TAtom a) = map toLower a
  show (TList [] _) = "[]"
  show (TList hs t) =
    "[" ++ intercalate ", " (map show hs) ++ maybe "" (\x -> " | " ++ show x) t ++ "]"
  show (TPred "\\=" (map show -> [ a, b ])) = a ++ " \\= " ++ b
  show (TPred h (map show -> args)) = h ++ "(" ++ intercalate ", " args ++ ")"

plWildcard :: Term
plWildcard = TVar "_"

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

typeAtom :: Type -> Term
typeAtom = TAtom . show

resolvable :: Term -> Term -> Term -> Term
resolvable ns t n = TPred "resolvable" [ ns, t, n ]

neq :: Term -> Term -> Term
neq a b = TPred "\\=" [ a, b ]

nameWithSynthesis :: Name (NameConstraint String) -> Fresh (Term, [Term])
nameWithSynthesis (Name (l NE.:| ls)) = case l of
  Wildcard cs -> do
    h <- TVar <$> fresh "H"
    t <- TVar <$> fresh "T"
    return (TList (reverse (h : exactlyNatoms ls)) (Just t), neqs h cs)
  _ -> return (TList (reverse (exactlyNatoms (l:ls))) Nothing, [])
  where
    neqs :: Term -> [String] -> [Term]
    neqs var = map (neq var . TAtom)

    exactlyNatoms :: [NameConstraint String] -> [Term]
    exactlyNatoms [] = []
    exactlyNatoms (Wildcard _ : t) = dnsWildcard : exactlyNatoms t
    exactlyNatoms (Exactly "" : t) = exactlyNatoms t
    exactlyNatoms (Exactly c : t) = TAtom c : exactlyNatoms t

nameTerm :: Name String -> Term
nameTerm (Name (NE.toList -> ls)) = TList (reverse $ map TAtom $ filter (not . null) ls) Nothing

data Rule = Rule
  { rhead :: Term
  , rbody :: [Term] }

instance Show Rule where
  show (Rule h []) = show h ++ "."
  show (Rule h b) = show h ++ " :- " ++ intercalate ", " (map show b) ++ "."
