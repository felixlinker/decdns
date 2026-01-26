{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
module Resolvable.Prolog
  ( Term(..)
  , emptyList
  , walk
  , renamePred
  , subterm
  , Rule(..)
  , mapRule
  , (Resolvable.Prolog.^)
  , dnsWildcard
  , typeAtom
  , (Resolvable.Prolog.\=)
  , nameWithSynthesis
  , nameTerm
  , showKnowledgeBase ) where

import NameConstraint
import ZoneFile

import Data.List (intercalate, groupBy, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, toUpper)
import Data.Semigroup (Semigroup(..), sconcat)
import Util.Fresh (Fresh, fresh)
import Data.Maybe (maybeToList)

data Term = TOp String [Term]
          | TVar String
          | TWildcard
          | TAtomQuoted String
          | TAtom String
          | TList [Term] (Maybe Term)
          | TString String
          | TPred String [Term]
  deriving (Eq, Ord)

emptyList :: Term
emptyList = TList [] Nothing

instance Show Term where
  show (TOp "^" [ a, b ]) = show a ++ "^" ++ show b
  show (TOp op (map show -> args)) = op ++ "(" ++ intercalate ", " args ++ ")"
  show (TVar "") = undefined
  show (TVar (h:t)) = toUpper h : t
  show TWildcard = "_"
  show (TAtom "") = undefined
  show (TAtom (h:t)) = toLower h : t
  show (TAtomQuoted a) =  "'" ++ a ++ "'"
  show (TList hs t) =
    "[" ++ intercalate ", " (map show hs) ++ maybe "" (\x -> " | " ++ show x) t ++ "]"
  show (TString s) = show s
  show (TPred "\\=" (map show -> [ a, b ])) = a ++ " \\= " ++ b
  show (TPred h (map show -> args)) = h ++ "(" ++ intercalate ", " args ++ ")"

walk :: Semigroup a => (Term -> a) -> Term -> a
walk f t@(TOp _ args) = sconcat (f t NE.:| map (walk f) args)
walk f t@(TVar _) = f t
walk f TWildcard = f TWildcard
walk f t@(TAtomQuoted _) = f t
walk f t@(TAtom _) = f t
walk f t@(TList hs tl) = sconcat (f t NE.:| map (walk f) (hs ++ maybeToList tl))
walk f t@(TString _) = f t
walk f t@(TPred _ args) = sconcat (f t NE.:| map (walk f) args)

replace :: (Term -> Term) -> Term -> Term
replace f t = rec (f t)
  where
    rec (TOp op ts) = TOp op (map (replace f) ts)
    rec u@(TVar _) = u
    rec u@TWildcard = u
    rec u@(TAtomQuoted _) = u
    rec u@(TAtom _) = u
    rec (TList ts tl) = TList (map (replace f) ts) tl
    rec u@(TString _) = u
    rec (TPred p ts) = TPred p (map (replace f) ts)

renamePred :: String -> String -> Term -> Term
renamePred from to = replace go
  where
    go :: Term -> Term
    go t@(TPred n args)
      | n == from = TPred to args
      | otherwise = t
    go t = t

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

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

typeAtom :: Type -> Term
typeAtom = TAtom . map toLower . show

(\=) :: Term -> Term -> Term
(\=) a b = TPred "\\=" [ a, b ]

nameWithSynthesis :: ConstrainedName -> Fresh (Term, [Term])
nameWithSynthesis (Name (l NE.:| ls)) = case l of
  Wildcard [] -> do
    return (TList (reverse (TWildcard : exactlyNatoms ls)) (Just TWildcard), [])
  Wildcard cs -> do
    h <- TVar <$> fresh "H"
    return (TList (reverse (h : exactlyNatoms ls)) (Just TWildcard), neqs h cs)
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
  deriving (Eq, Ord)

instance Functor Rule where
  fmap f (Rule a h b) = Rule (f a) h b

instance Show (Rule a) where
  show (Rule _ h []) = show h ++ "."
  show (Rule _ h b) = show h ++ " :- " ++ intercalate ", " (map show b) ++ "."

mapRule :: (Term -> Term) -> Rule a -> Rule a
mapRule f (Rule a h b) = Rule a (f h) (map f b)

showKnowledgeBase :: [Rule a] -> String
showKnowledgeBase kb =
  let grouped = groupBy (\r1 r2 -> headName r1 == headName r2) kb
      -- Note that we use negation because facts will have @not . null . rbody@
      -- be @False@, which is ordered before @True@.
      factsFirst = map (sortOn (not . null . rbody)) grouped
  in unlines $ map show $ concat factsFirst
  where
    headName :: Rule a -> String
    headName (Rule _ (TPred n _) _) = n
    headName _ = undefined
