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
  , member
  , walk
  , renameFunctor
  , subterm
  , Rule(..)
  , mapRule
  , (Resolvable.Prolog.^)
  , dnsWildcard
  , ttypeAtom
  , typeAtom
  , b64Atom
  , algAtom
  , keyData
  , pkRefData
  , dataFunctor
  , (\=)
  , (=<)
  , (\+)
  , resolves
  , resolvable
  , ad
  , rrsigValid
  , digestValid
  , nameWithSynthesis
  , nameWithoutSynthesis
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
import DNS.SEC.Crypto
import DNS.SEC.Keys
import Util.Bytes (toB64)
import qualified Data.ByteString as BS

caretSym :: String
caretSym = "^"

listSym :: String
listSym = "."

memberSym :: String
memberSym = "member"

neqSym :: String
neqSym = "\\="

leqSym :: String
leqSym = "=<"

negSym :: String
negSym = "\\+"

resolvesSym :: String
resolvesSym = "resolves"

resolvableSym :: String
resolvableSym = "resolvable"

adSym :: String
adSym = "ad"

ipSym :: String
ipSym = "ip"

nameSym :: String
nameSym = "name"

textSym :: String
textSym = "text"

mxSym :: String
mxSym = "mx"

soaSym :: String
soaSym = "soa"

keySym :: String
keySym = "key"

sigSym :: String
sigSym = "sig"

nsecSym :: String
nsecSym = "nsec"

pkRefSym :: String
pkRefSym = "pkref"

rrsigValidSym :: String
rrsigValidSym = "rrsigValid"

digestValidSym :: String
digestValidSym = "digestValid"

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
  deriving (Eq, Ord)

data TermView = TVList Term Term
              | TVMember Term Term
              | TVNeq Term Term
              | TVLeq Term Term
              | TVResolves Term Term Term Term Term
              | TVResolvable Term Term Term
              | TVRRSIGValid Term Term Term Term Term Term Term
              | TVIP Term
              | TVName Term
              | TVText Term
              | TVMX Term Term
              | TVSOA Term Term Term Term Term Term Term
              | TVKey Term Term Term Term
              | TVSig Term Term Term Term Term Term Term Term Term
              | TVNSEC Term Term
              | TVPKRef Term Term Term Term
              | TVAd Bool
              | TVFunctor String [ Term ]
              | TVEmptyList
              | TVVar String
              | TVWildcard
              | TVAtomQuoted String
              | TVAtom String
              | TVString String
              | TVInt Int

toView :: Term -> TermView
toView (TFunctor n args)
  | n == listSym = case args of
    [ a, b ] -> TVList a b
    _ -> undefined
  | n == memberSym = case args of
    [ a, b ] -> TVMember a b
    _ -> undefined
  | n == neqSym = case args of
    [ a, b ] -> TVNeq a b
    _ -> undefined
  | n == leqSym = case args of
    [ a, b ] -> TVLeq a b
    _ -> undefined
  | n == resolvesSym = case args of
    [ ns, qtype, qname, d, adb ] -> TVResolves ns qtype qname d adb
    _ -> undefined
  | n == resolvableSym = case args of
    [ ns, qtype, qname ] -> TVResolvable ns qtype qname
    _ -> undefined
  | n == rrsigValidSym = case args of
    [ nam, typeCovered, orgTTL, rrset, ls, sig, key ] -> TVRRSIGValid nam typeCovered orgTTL rrset ls sig key
    _ -> undefined
  | n == ipSym = case args of
    [ a ] -> TVIP a
    _ -> undefined
  | n == nameSym = case args of
    [ a ] -> TVName a
    _ -> undefined
  | n == textSym = case args of
    [ a ] -> TVText a
    _ -> undefined
  | n == mxSym = case args of
    [ a, b ] -> TVMX a b
    _ -> undefined
  | n == soaSym = case args of
    [ a, b, c, d, e, f, g ] -> TVSOA a b c d e f g
    _ -> undefined
  | n == keySym = case args of
    [ a, b, c, d ] -> TVKey a b c d
    _ -> undefined
  | n == sigSym = case args of
    [ a, b, c, d, e, f, g, h, i ] -> TVSig a b c d e f g h i
    _ -> undefined
  | n == nsecSym = case args of
    [ a, b ] -> TVNSEC a b
    _ -> undefined
  | n == pkRefSym = case args of
    [ a, b, c, d ] -> TVPKRef a b c d
    _ -> undefined
  | n == adSym = case map toView args of
    [ TVInt 0 ] -> TVAd False
    [ TVInt 1 ] -> TVAd True
    _ -> undefined
  | otherwise = TVFunctor n args
toView TEmptyList = TVEmptyList
toView (TVar v) = TVVar v
toView TWildcard = TVWildcard
toView (TAtomQuoted n) = TVAtomQuoted n
toView (TAtom n) = TVAtom n
toView (TString s) = TVString s
toView (TInt i) = TVInt i

fromView :: TermView -> Term
fromView (TVList a b) = TFunctor listSym [ a, b ]
fromView (TVMember a b) = member a b
fromView (TVNeq a b) = a \= b
fromView (TVLeq a b) = a =< b
fromView (TVResolves ns qtype qname d adb) = resolves ns qtype qname d adb
fromView (TVResolvable ns qtype qname) = resolvable ns qtype qname
fromView (TVRRSIGValid nam typeCovered orgTTL rrset ls sig key) = rrsigValid nam typeCovered orgTTL rrset ls sig key
fromView (TVIP a) = ipData a
fromView (TVName a) = nameData a
fromView (TVText a) = textData a
fromView (TVMX a b) = mxData a b
fromView (TVSOA a b c d e f g) = soaData a b c d e f g
fromView (TVKey a b c d) = keyData a b c d
fromView (TVSig a b c d e f g h i) = sigData a b c d e f g h i
fromView (TVNSEC a b) = nsecData a b
fromView (TVPKRef a b c d) = pkRefData a b c d
fromView (TVAd b) = ad (fromEnum b)
fromView (TVFunctor n args) = TFunctor n args
fromView TVEmptyList = TEmptyList
fromView (TVVar v) = TVar v
fromView TVWildcard = TWildcard
fromView (TVAtomQuoted n) = TAtomQuoted n
fromView (TVAtom n) = TAtom n
fromView (TVString s) = TString s
fromView (TVInt i) = TInt i

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

listWithTail :: [Term] -> Term -> Term
listWithTail xs t = go xs
  where
    go :: [Term] -> Term
    go [] = t
    go (a:as) = TFunctor listSym [ a, go as ]

collectListWithTail :: Term -> ([Term], Maybe Term)
collectListWithTail = swap . rec . toView
  where
    rec :: TermView -> (Maybe Term, [Term])
    rec (TVList a b) = (a:) <$> rec (toView b)
    rec TVEmptyList = (Nothing, [])
    rec t = (Just (fromView t), [])

collectList :: Term -> [Term]
collectList = fst . collectListWithTail

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

walk :: Semigroup a => (Term -> a) -> Term -> a
walk f t@(TFunctor _ args) = sconcat (f t NE.:| map (walk f) args)
walk f TEmptyList = f TEmptyList
walk f t@(TVar _) = f t
walk f TWildcard = f TWildcard
walk f t@(TAtomQuoted _) = f t
walk f t@(TAtom _) = f t
walk f t@(TString _) = f t
walk f t@(TInt _) = f t

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
typeAtom = ttypeAtom . toType

algAtom :: AlgorithmType -> Term
algAtom = TInt . algorithmToInt

b64Atom :: BS.ByteString -> Term
b64Atom = TString . toB64

digAtom :: DigestType -> Term
digAtom = TInt . digestToInt

ipData :: Term -> Term
ipData a = TFunctor ipSym [ a ]

nameData :: Term -> Term
nameData a = TFunctor nameSym [ a ]

textData :: Term -> Term
textData a = TFunctor textSym [ a ]

mxData :: Term -> Term -> Term
mxData a b = TFunctor mxSym [ a, b ]

soaData :: Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term
soaData a b c d e f g = TFunctor soaSym [ a, b, c, d, e, f, g ]

keyData :: Term -> Term -> Term -> Term -> Term
keyData a b c d = TFunctor keySym [ a, b, c, d ]

sigData :: Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term
sigData a b c d e f g h i = TFunctor sigSym [ a, b, c, d, e, f, g, h, i ]

nsecData :: Term -> Term -> Term
nsecData a b = TFunctor nsecSym [ a, b ]

pkRefData :: Term -> Term -> Term -> Term -> Term
pkRefData a b c d = TFunctor pkRefSym [ a, b, c, d ]

dataFunctor :: Type -> Term
dataFunctor (A s) = ipData (TString s)
dataFunctor (CNAME n) = nameData (nameTerm n)
dataFunctor (DNAME n) = nameData (nameTerm n)
dataFunctor (NS n) = nameData (nameTerm n)
dataFunctor (AAAA s) = ipData (TString s)
dataFunctor (TXT s) = textData (TString s)
dataFunctor (MX p n) = mxData (TInt p) (nameTerm n)
dataFunctor (SOA mn rn s rf rt e m) = soaData (nameTerm mn) (nameTerm rn) (TInt s) (TInt rf) (TInt rt) (TInt e) (TInt m)
dataFunctor r@(DNSKEY fs alg k) = keyData (TInt (dnsKeyFlagsToInt fs)) (algAtom alg) (b64Atom k) (TInt (fromIntegral $ dnskeyKeyTag r))
dataFunctor (RRSIG ty alg ls ottl sigExp sigInc tag sigName sig) = sigData (ttypeAtom ty) (algAtom alg) (TInt ls) (TInt ottl) (TInt sigExp) (TInt sigInc) (TInt tag) (nameTerm sigName) (b64Atom sig)
dataFunctor (NSEC nextN covered) = nsecData (nameTerm nextN) (list (map ttypeAtom covered))
dataFunctor (DS tag alg digT dig) = pkRefData (TInt tag) (algAtom alg) (digAtom digT) (b64Atom dig)

(\=) :: Term -> Term -> Term
(\=) = tBin neqSym

(=<) :: Term -> Term -> Term
(=<) = tBin leqSym

(\+) :: Term -> Term
(\+) t1 = TFunctor negSym [t1]

resolves :: Term -> Term -> Term -> Term -> Term -> Term
resolves ns qtype qname d adb = TFunctor resolvesSym [ ns, qtype, qname, d, adb ]

resolvable :: Term -> Term -> Term -> Term
resolvable ns qtype qname = TFunctor resolvableSym [ ns, qtype, qname ]

ad :: Int -> Term
ad = TFunctor adSym . (:[]) . TInt

rrsigValid :: Term -> Term -> Term -> Term -> Term -> Term -> Term-> Term
rrsigValid nam typeCovered origTTL rrset ls sig key  = TFunctor rrsigValidSym [ nam, typeCovered, origTTL, rrset, ls, sig, key ]

-- TODO: Create term view
digestValid :: Term -> Term -> Term -> Term -> Term -> Term -> Term
digestValid typ nam flags alg key dig = TFunctor digestValidSym [ typ, nam, flags, alg, key, dig ]

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

nameWithoutSynthesis :: ConstrainedName -> Term
nameWithoutSynthesis = nameTerm . toName

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
