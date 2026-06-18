{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
module Resolvable.Prolog.Core
  -- TODO: Enforce usage of termView or exported constructors by hiding constructors of Term
  ( Term(..)
  , freshVar
  , freshVars2
  , freshVars3
  , freshVars4
  , freshVars5
  , freshVars6
  , replace
  , equiv
  , emptyList
  , tList
  , list
  , listWithTail
  , collectList
  , collectListWithTail
  , member
  , walk
  , renameFunctor
  , subterm
  , Rule(..)
  , fact
  , mapRule
  , (Resolvable.Prolog.Core.^)

  , (\=)
  , (=<)
  , (\+)
  , showKnowledgeBase

  , dnskeyType
  , dsType
  , aType
  , aaaaType
  , cnameType
  , dnameType
  , nsType
  , dnskey
  , ds
  , dnsWildcard
  , ttypeTerm
  , typeTerm
  , b64Term
  , algTerm
  , rrsetTerm
  , dataTerm
  , resolves
  , resolvable
  , ad
  , rrsigValid
  , digestValid
  , keyTag
  , nameWithSynthesis
  , nameWithoutSynthesis
  , nameTerm ) where

import DNS.Name
import DNS.Record
import DNS.SEC.Crypto ( algorithmToInt, digestToInt, AlgorithmType, DigestType )
import DNS.SEC.Keys (dnsKeyFlagsToInt)
import qualified DNS.RecordType as T
import NameConstraint
import Resolvable.Prolog.Symbols
import Util.Fresh (Fresh, fresh)
import Util.List (zipWithStrict)
import Util.Bytes (toB64)

import qualified Data.ByteString as BS
import Data.List (intercalate, groupBy, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, toUpper)
import Data.Semigroup (Semigroup(..), sconcat)
import Data.Tuple (swap)
import Data.Maybe (isNothing)

infixFunctors :: [String]
infixFunctors =
  [ caretSym
  , neqSym
  , leqSym
  , negSym ]

data Term = TFunctor String [Term]
          | TEmptyList
          | TVar String
          | TWildcard
          | TAtomQuoted String
          | TAtom String
          | TString String
          | TInt Int
          | TComplex Type
  deriving (Eq, Ord)

freshVar :: String -> Fresh Term
freshVar = fmap TVar . fresh

freshVars2 :: (String, String) -> Fresh (Term, Term)
freshVars2 (a, b) = do
  va <- freshVar a
  vb <- freshVar b
  return (va, vb)

freshVars3 :: (String, String, String) -> Fresh (Term, Term, Term)
freshVars3 (a, b, c) = do
  va <- freshVar a
  vb <- freshVar b
  vc <- freshVar c
  return (va, vb, vc)

freshVars4 :: (String, String, String, String) -> Fresh (Term, Term, Term, Term)
freshVars4 (a, b, c, d) = do
  va <- freshVar a
  vb <- freshVar b
  vc <- freshVar c
  vd <- freshVar d
  return (va, vb, vc, vd)

freshVars5 :: (String, String, String, String, String) -> Fresh (Term, Term, Term, Term, Term)
freshVars5 (a, b, c, d, e) = do
  va <- freshVar a
  vb <- freshVar b
  vc <- freshVar c
  vd <- freshVar d
  ve <- freshVar e
  return (va, vb, vc, vd, ve)

freshVars6 :: (String, String, String, String, String, String) -> Fresh (Term, Term, Term, Term, Term, Term)
freshVars6 (a, b, c, d, e, f) = do
  va <- freshVar a
  vb <- freshVar b
  vc <- freshVar c
  vd <- freshVar d
  ve <- freshVar e
  vf <- freshVar f
  return (va, vb, vc, vd, ve, vf)

tBin :: String -> Term -> Term -> Term
tBin n a b = TFunctor n [ a, b ]

tList :: Term -> Term -> Term
tList = tBin listSym

emptyList :: Term
emptyList = TEmptyList

list :: Foldable t =>  t Term -> Term
list ts
  | null ts = TEmptyList
  | otherwise = foldr (\x acc -> TFunctor listSym [ x, acc ]) TEmptyList ts

collectListWithTail :: Term -> ([Term], Maybe Term)
collectListWithTail = swap . rec
  where
    rec :: Term -> (Maybe Term, [Term])
    rec (TFunctor sym [a, b])
      | sym == listSym = (a:) <$> rec b
    rec TEmptyList = (Nothing, [])
    rec t = (Just t, [])

collectList :: Term -> Maybe [Term]
collectList t =
  let (l, mt) = collectListWithTail t
  -- Enforce that tail is empty
  in if isNothing mt then Just l else Nothing

listWithTail :: [Term] -> Term -> Term
listWithTail xs t = go xs
  where
    go :: [Term] -> Term
    go [] = t
    go (a:as) = TFunctor listSym [ a, go as ]

member :: Term -> Term -> Term
member a b = TFunctor memberSym [ a, b ]

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
  show (TInt i) = show i
  -- "Flatten" data for compilation to Prolog. We only maintain TComplex for
  -- reasoning purposes.
  show (TComplex dat) = show $ dataTerm dat

walk :: Semigroup a => (Term -> a) -> Term -> a
walk f t@(TFunctor _ args) = sconcat (f t NE.:| map (walk f) args)
walk f TEmptyList = f TEmptyList
walk f t@(TVar _) = f t
walk f TWildcard = f TWildcard
walk f t@(TAtomQuoted _) = f t
walk f t@(TAtom _) = f t
walk f t@(TString _) = f t
walk f t@(TInt _) = f t
walk f t@(TComplex _) = f t

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
    rec u@(TInt _) = u
    rec u@(TComplex _) = u

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

(\=) :: Term -> Term -> Term
(\=) = tBin neqSym

(=<) :: Term -> Term -> Term
(=<) = tBin leqSym

(\+) :: Term -> Term
(\+) t1 = TFunctor negSym [t1]

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

fact :: a -> Term -> Rule a
fact ann h = Rule ann h []

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

ttypeSym :: T.Type -> String
ttypeSym = map toLower . show

dnskeyType :: Term
dnskeyType = ttypeTerm T.DNSKEY

dsType :: Term
dsType = ttypeTerm T.DS

aType :: Term
aType = ttypeTerm T.A

aaaaType :: Term
aaaaType = ttypeTerm T.AAAA

cnameType :: Term
cnameType = ttypeTerm T.CNAME

dnameType :: Term
dnameType = ttypeTerm T.DNAME

nsType :: Term
nsType = ttypeTerm T.NS

dnskey :: Term -> Term -> Term -> Term
dnskey a b c = TFunctor (ttypeSym T.DNSKEY) [a, b, c]

ds :: Term -> Term -> Term -> Term -> Term
ds a b c d = TFunctor (ttypeSym T.DS) [a, b, c, d]

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

ttypeTerm :: T.Type -> Term
ttypeTerm = TAtom . map toLower . show

typeTerm :: Type -> Term
typeTerm = ttypeTerm . toType

algTerm :: AlgorithmType -> Term
algTerm = TInt . algorithmToInt

b64Term :: BS.ByteString -> Term
b64Term = TString . toB64

digestTerm :: DigestType -> Term
digestTerm = TInt . digestToInt

rrsetTerm :: (Foldable f, Functor f) => f RData -> Term
rrsetTerm = list . fmap (TComplex . rtype)

dataTerm :: Type -> Term
dataTerm t = TFunctor (ttypeSym $ toType t) $ case t of
  A s -> [TAtomQuoted $ show s]
  AAAA s -> [TAtomQuoted $ show s]
  CNAME n -> [nameTerm n]
  DNAME n -> [nameTerm n]
  NS n -> [nameTerm n]
  TXT s -> [TAtomQuoted s]
  MX p n -> [TInt p, nameTerm n]
  SOA mn rn s rf rt e m -> [nameTerm mn, nameTerm rn, TInt s, TInt rf, TInt rt, TInt e, TInt m]
  DNSKEY fs alg k -> [TInt (dnsKeyFlagsToInt fs), algTerm alg, b64Term k]
  RRSIG (RRSIGData ty alg ls ottl sigExp sigInc tag sigName sig) -> [ttypeTerm ty, algTerm alg, TInt ls, TInt ottl, TInt sigExp, TInt sigInc, TInt tag, nameTerm sigName, b64Term sig]
  NSEC nextN covered -> [nameTerm nextN, list (map ttypeTerm covered)]
  DS tag alg digT dig -> [TInt tag, algTerm alg, digestTerm digT, b64Term dig]

resolves :: Term -> Term -> Term -> Term -> Term -> Term
resolves ns qtype qname d adb = TFunctor resolvesSym [ ns, qtype, qname, d, adb ]

resolvable :: Term -> Term -> Term -> Term
resolvable ns qtype qname = TFunctor resolvableSym [ ns, qtype, qname ]

ad :: Int -> Term
ad = TFunctor adSym . (:[]) . TInt

rrsigValid :: Term -> Term -> Term -> Term -> Term
rrsigValid nam dat sig key  = TFunctor rrsigValidSym [ nam, dat, sig, key ]

-- TODO: Create term view
digestValid :: Term -> Term -> Term -> Term -> Term -> Term -> Term
digestValid typ nam flags alg key dig = TFunctor digestValidSym [ typ, nam, flags, alg, key, dig ]

keyTag :: Term -> Term -> Term
keyTag d tag = TFunctor keyTagSym [ d, tag ]

nameWithSynthesis :: ConstrainedName -> Fresh (Term, [Term])
nameWithSynthesis (Name (l NE.:| ls)) = case l of
  Wildcard [] -> do
    return (listWithTail (reverse (TWildcard : exactlyNatoms ls)) TWildcard, [])
  Wildcard cs -> do
    h <- TVar <$> fresh "H"
    return (listWithTail (reverse (h : exactlyNatoms ls)) TWildcard, neqs h cs)
  _ -> return (list (reverse (exactlyNatoms (l:ls))), [])
  where
    neqs :: Term -> [String] -> [Term]
    neqs var = map ((var \=) . TAtom)

    exactlyNatoms :: [LabelConstraint String] -> [Term]
    exactlyNatoms [] = []
    exactlyNatoms (Wildcard _ : t) = dnsWildcard : exactlyNatoms t
    exactlyNatoms (Exactly "" : t) = exactlyNatoms t
    exactlyNatoms (Exactly c : t) = TAtom c : exactlyNatoms t

nameTerm :: Name String -> Term
nameTerm (Name (NE.toList -> ls)) = list (reverse $ map TAtom $ filter (not . null) ls)

nameWithoutSynthesis :: ConstrainedName -> Term
nameWithoutSynthesis = nameTerm . toName
