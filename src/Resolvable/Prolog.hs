{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
module Resolvable.Prolog
  ( Term(..)
  , walk
  , subterm
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
import Data.Semigroup (Semigroup(..), sconcat)
import Util.Fresh (Fresh, fresh)

data Term = TOp String [Term] | TVar String | TAtomQuoted String | TAtom String | TList [Term] (Maybe String) | TString String | TPred String [Term]
  deriving (Eq, Ord)

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
    "[" ++ intercalate ", " (map show hs) ++ maybe "" (\x -> " | " ++ show (TVar x)) t ++ "]"
  show (TString s) = show s
  show (TPred "\\=" (map show -> [ a, b ])) = a ++ " \\= " ++ b
  show (TPred h (map show -> args)) = h ++ "(" ++ intercalate ", " args ++ ")"

walk :: Semigroup a => (Term -> a) -> Term -> a
walk f t@(TOp _ args) = sconcat (f t NE.:| map (walk f) args)
walk f t@(TVar _) = f t
walk f t@(TAtomQuoted _) = f t
walk f t@(TAtom _) = f t
walk f t@(TList hs tl) = sconcat (f t NE.:| map (walk f) (hs ++ maybe [] ((:[]) . TVar) tl))
walk f t@(TString _) = f t
walk f t@(TPred _ args) = sconcat (f t NE.:| map (walk f) args)

newtype Or = Or Bool

toBool :: Or -> Bool
toBool (Or b) = b

instance Semigroup Or where
  Or True <> _ = Or True
  _ <> o = o

instance Monoid Or where
  mempty :: Or
  mempty = Or False

subterm :: Term -> Term -> Bool
subterm t1 t2 = toBool $ walk (\t -> Or (t == t1)) t2

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
    t <- fresh "T"
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
