{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Prolog
  -- TODO: Enforce usage of termView or exported constructors by hiding constructors of Term
  ( Term(..)
  , TermView(..)
  , toView
  , fromView
  , equiv
  , emptyList
  , tList
  , list
  , listWithTail
  , collectList
  , collectListWithTail
  , walk
  , renameFunctor
  , subterm
  , Rule(..)
  , mapRule
  , (Resolvable.Prolog.^)
  , dnsWildcard
  , ttypeAtom
  , typeAtom
  , dataAtom
  , (Resolvable.Prolog.\=)
  , (\+)
  , nameWithSynthesis
  , nameTerm
  , showKnowledgeBase ) where

import NameConstraint
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T

import Data.List (intercalate, groupBy, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, toUpper)
import Data.Semigroup (Semigroup(..), sconcat)
import Util.Fresh (Fresh, fresh)
import Util.List (zipWithStrict)
import Data.Tuple (swap)
import Data.Bifunctor (Bifunctor(bimap))

caretSym :: String
caretSym = "^"

listSym :: String
listSym = "."

neqSym :: String
neqSym = "\\="

negSym :: String
negSym = "\\+"

infixFunctors :: [String]
infixFunctors =
  [ caretSym
  , neqSym
  , negSym ]

data Term = TFunctor String [Term]
          | TEmptyList
          | TVar String
          | TWildcard
          | TAtomQuoted String
          | TAtom String
          | TString String
  deriving (Eq, Ord)

data TermView = TVList TermView TermView
              | TVNeq TermView TermView
              | TVFunctor String [ TermView ]
              | TVEmptyList
              | TVVar String
              | TVWildcard
              | TVAtomQuoted String
              | TVAtom String
              | TVString String

toView :: Term -> TermView
toView (TFunctor n args)
  | n == listSym = case map toView args of
    [ a, b ] -> TVList a b
    _ -> undefined
  | n == neqSym = case map toView args of
    [ a, b ] -> TVNeq a b
    _ -> undefined
  | otherwise = TVFunctor n (map toView args)
toView TEmptyList = TVEmptyList
toView (TVar v) = TVVar v
toView TWildcard = TVWildcard
toView (TAtomQuoted n) = TVAtomQuoted n
toView (TAtom n) = TVAtom n
toView (TString s) = TVString s

fromView :: TermView -> Term
fromView (TVList a b) = TFunctor listSym [ fromView a, fromView b ]
fromView (TVNeq a b) = TFunctor neqSym [ fromView a, fromView b ]
fromView (TVFunctor n args) = TFunctor n (map fromView args)
fromView TVEmptyList = TEmptyList
fromView (TVVar v) = TVar v
fromView TVWildcard = TWildcard
fromView (TVAtomQuoted n) = TAtomQuoted n
fromView (TVAtom n) = TAtom n
fromView (TVString s) = TString s

tBin :: String -> Term -> Term -> Term
tBin n a b = TFunctor n [ a, b ]

tList :: Term -> Term -> Term
tList = tBin listSym

emptyList :: Term
emptyList = TEmptyList

list :: [Term] -> Term
list [] = TEmptyList
list (x:xs) = TFunctor listSym [ x, list xs ]

listWithTail :: [Term] -> Term -> Term
listWithTail xs t = go xs
  where
    go :: [Term] -> Term
    go [] = t
    go (a:as) = TFunctor listSym [ a, go as ]

collectListWithTail :: Term -> ([Term], Maybe Term)
collectListWithTail = swap . bimap (fmap fromView) (fmap fromView) . rec . toView
  where
    rec :: TermView -> (Maybe TermView, [TermView])
    rec (TVList a b) = (a:) <$> rec b
    rec TVEmptyList = (Nothing, [])
    rec t = (Just t, [])

collectList :: Term -> [Term]
collectList = fst . collectListWithTail

equiv :: Term -> Term -> Bool
equiv (TFunctor a argsa) (TFunctor b argsb) = a == b && maybe False and (zipWithStrict equiv argsa argsb)
equiv TEmptyList TEmptyList = True
equiv (TVar a) (TVar b) = a == b
equiv TWildcard _ = True
equiv _ TWildcard = True
equiv (TAtomQuoted a) (TAtomQuoted b) = a == b
equiv (TAtom a) (TAtom b) = a == b
  -- where
  --   tailEqual :: Maybe Term -> Maybe Term -> Bool
  --   tailEqual Nothing Nothing = True
  --   tailEqual (Just ta) (Just tb) = equiv ta tb
  --   tailEqual _ _ = False
equiv (TString a) (TString b) = a == b
equiv _ _ = False

instance Show Term where
  show t@(TFunctor op args)
    | op `elem` infixFunctors =
      case args of
        [ a ] -> op ++ " " ++ show a
        [ a, b ] -> show a ++ " " ++ op ++ " " ++ show b
        _ -> undefined
    | op == listSym =
      let (l, tl) = collectListWithTail t
      in "[" ++ intercalate ", " (map show l) ++ maybe "" (\x -> " | " ++ show x) tl ++ "]"
    | otherwise = op ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show TEmptyList = "[]"
  show (TVar "") = undefined
  show (TVar (h:t)) = toUpper h : t
  show TWildcard = "_"
  show (TAtom "") = undefined
  show (TAtom (h:t)) = toLower h : t
  show (TAtomQuoted a) =  "'" ++ a ++ "'"
  show (TString s) = show s

walk :: Semigroup a => (Term -> a) -> Term -> a
walk f t@(TFunctor _ args) = sconcat (f t NE.:| map (walk f) args)
walk f TEmptyList = f TEmptyList
walk f t@(TVar _) = f t
walk f TWildcard = f TWildcard
walk f t@(TAtomQuoted _) = f t
walk f t@(TAtom _) = f t
walk f t@(TString _) = f t

replace :: (Term -> Term) -> Term -> Term
replace f t = rec (f t)
  where
    rec (TFunctor op ts) = TFunctor op (map (replace f) ts)
    rec u@TEmptyList = u
    rec u@(TVar _) = u
    rec u@TWildcard = u
    rec u@(TAtomQuoted _) = u
    rec u@(TAtom _) = u
    rec u@(TString _) = u

renameFunctor :: String -> String -> Term -> Term
renameFunctor from to = replace go
  where
    go :: Term -> Term
    go t@(TFunctor n args)
      | n == from = TFunctor to args
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
(^) = tBin caretSym

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

ttypeAtom :: T.Type -> Term
ttypeAtom = TAtom . map toLower . show

typeAtom :: Type -> Term
typeAtom = TAtom . map toLower . show . toType

dataAtom :: Type -> Term
dataAtom = TString . showData

(\=) :: Term -> Term -> Term
(\=) = tBin neqSym

(\+) :: Term -> Term
(\+) t1 = TFunctor negSym [t1]

nameWithSynthesis :: ConstrainedName -> Fresh (Term, [(Term, Term)])
nameWithSynthesis (Name (l NE.:| ls)) = case l of
  Wildcard [] -> do
    return (listWithTail (reverse (TWildcard : exactlyNatoms ls)) TWildcard, [])
  Wildcard cs -> do
    h <- TVar <$> fresh "H"
    return (listWithTail (reverse (h : exactlyNatoms ls)) TWildcard, neqs h cs)
  _ -> return (list (reverse (exactlyNatoms (l:ls))), [])
  where
    neqs :: Term -> [String] -> [(Term, Term)]
    neqs var = map ((var,) . TAtom)

    exactlyNatoms :: [LabelConstraint String] -> [Term]
    exactlyNatoms [] = []
    exactlyNatoms (Wildcard _ : t) = dnsWildcard : exactlyNatoms t
    exactlyNatoms (Exactly "" : t) = exactlyNatoms t
    exactlyNatoms (Exactly c : t) = TAtom c : exactlyNatoms t

nameTerm :: Name String -> Term
nameTerm (Name (NE.toList -> ls)) = list (reverse $ map TAtom $ filter (not . null) ls)

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
    headName (Rule _ (TFunctor n _) _) = n
    headName _ = undefined
