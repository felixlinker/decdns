{-# LANGUAGE ViewPatterns #-}
module Resolvable.Prolog
  ( Term(..)
  , Rule(..)
  , (Resolvable.Prolog.^)
  , plWildcard
  , dnsWildcard
  , typeAtom
  , (Resolvable.Prolog.\=)
  , nameWithSynthesis
  , nameTerm ) where

import NameConstraint
import ZoneFile

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, toUpper)
import Util.Fresh (Fresh, fresh)

data Term = TOp String [Term] | TVar String | TAtomQuoted String | TAtom String | TList [Term] (Maybe Term) | TString String | TPred String [Term]

instance Show Term where
  show (TOp "^" [ a, b ]) = show a ++ "^" ++ show b
  show (TOp op (map show -> args)) = op ++ "(" ++ intercalate ", " args ++ ")"
  show (TVar "") = undefined
  show (TVar (h:t)) = toUpper h : t
  show (TAtom "") = undefined
  show (TAtom (h:t)) = toLower h : t
  show (TAtomQuoted a) =  "'" ++ a ++ "'"
  show (TList [] _) = "[]"
  show (TList hs t) =
    "[" ++ intercalate ", " (map show hs) ++ maybe "" (\x -> " | " ++ show x) t ++ "]"
  show (TString s) = show s
  show (TPred "\\=" (map show -> [ a, b ])) = a ++ " \\= " ++ b
  show (TPred h (map show -> args)) = h ++ "(" ++ intercalate ", " args ++ ")"

(^) :: Term -> Term -> Term
(^) a b = TOp "^" [ a, b ]

plWildcard :: Term
plWildcard = TVar "_"

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

typeAtom :: Type -> Term
typeAtom = TAtom . map toLower . show

(\=) :: Term -> Term -> Term
(\=) a b = TPred "\\=" [ a, b ]

nameWithSynthesis :: ConstrainedName -> Fresh (Term, [Term])
nameWithSynthesis (Name (l NE.:| ls)) = case l of
  Wildcard cs -> do
    h <- TVar <$> fresh "H"
    t <- TVar <$> fresh "T"
    return (TList (reverse (h : exactlyNatoms ls)) (Just t), neqs h cs)
  _ -> return (TList (reverse (exactlyNatoms (l:ls))) Nothing, [])
  where
    neqs :: Term -> [String] -> [Term]
    neqs var = map ((var \=) . TAtom)

    exactlyNatoms :: [NameConstraint String] -> [Term]
    exactlyNatoms [] = []
    exactlyNatoms (Wildcard _ : t) = dnsWildcard : exactlyNatoms t
    exactlyNatoms (Exactly "" : t) = exactlyNatoms t
    exactlyNatoms (Exactly c : t) = TAtom c : exactlyNatoms t

nameTerm :: Name String -> Term
nameTerm (Name (NE.toList -> ls)) = TList (reverse $ map TAtom $ filter (not . null) ls) Nothing

data Rule a = Rule
  { rannotation :: a
  , rhead :: Term
  , rbody :: [Term] }

instance Functor Rule where
  fmap f (Rule a h b) = Rule (f a) h b

instance Show (Rule a) where
  show (Rule _ h []) = show h ++ "."
  show (Rule _ h b) = show h ++ " :- " ++ intercalate ", " (map show b) ++ "."
