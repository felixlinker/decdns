{-# LANGUAGE ViewPatterns #-}
module Resolvable.Prolog
  ( Term(..)
  , Predicate(..)
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

data Term = TVar String | TAtom String | TList [Term] (Maybe Term)

instance Show Term where
  show (TVar v) = map toUpper v
  show (TAtom a) = map toLower a
  show (TList [] _) = "[]"
  show (TList hs t) =
    "[" ++ intercalate ", " (map show hs) ++ maybe "" (\x -> " | " ++ show x) t ++ "]"

plWildcard :: Term
plWildcard = TVar "_"

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

typeAtom :: Type -> Term
typeAtom = TAtom . show

data Predicate = Predicate String [Term]

instance Show Predicate where
  show (Predicate "\\=" (map show -> [ a, b ])) = a ++ " \\= " ++ b
  show (Predicate h (map show -> args)) = h ++ "(" ++ intercalate ", " args ++ ")"

resolvable :: Term -> Term -> Term -> Predicate
resolvable ns t n = Predicate "resolvable" [ ns, t, n ]

neq :: Term -> Term -> Predicate
neq a b = Predicate "\\=" [ a, b ]

nameWithSynthesis :: Name (NameConstraint String) -> Fresh (Term, [Predicate])
nameWithSynthesis (Name (l NE.:| ls)) = case l of
  Wildcard cs -> do
    h <- TVar <$> fresh "H"
    t <- TVar <$> fresh "T"
    return (TList (reverse (h : exactlyNatoms ls)) (Just t), neqs h cs)
  _ -> return (TList (reverse (exactlyNatoms (l:ls))) Nothing, [])
  where
    neqs :: Term -> [String] -> [Predicate]
    neqs var = map (neq var . TAtom)

    exactlyNatoms :: [NameConstraint String] -> [Term]
    exactlyNatoms [] = []
    exactlyNatoms (Wildcard _ : t) = dnsWildcard : exactlyNatoms t
    exactlyNatoms (Exactly "" : t) = exactlyNatoms t
    exactlyNatoms (Exactly c : t) = TAtom c : exactlyNatoms t

nameTerm :: Name String -> Term
nameTerm (Name (NE.toList -> ls)) = TList (reverse $ map TAtom $ filter (not . null) ls) Nothing

data Rule = Rule
  { rhead :: Predicate
  , rbody :: [Predicate] }

instance Show Rule where
  show (Rule h []) = show h ++ "."
  show (Rule h b) = show h ++ " :- " ++ intercalate ", " (map show b) ++ "."
