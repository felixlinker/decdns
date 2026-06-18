{-# LANGUAGE ViewPatterns #-}
module Resolvable.Prolog.Library
  ( dnskeyType
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

import Resolvable.Prolog.Symbols
import Resolvable.Prolog.Core
import DNS.Name
import qualified DNS.RecordType as T
import DNS.Record
import DNS.SEC.Crypto ( algorithmToInt, digestToInt, AlgorithmType, DigestType )
import DNS.SEC.Keys (dnsKeyFlagsToInt)
import NameConstraint
import Util.Fresh
import Util.Bytes (toB64)

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower)

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
rrsetTerm = list . fmap (dataTerm . rtype)

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
  RRSIG ty alg ls ottl sigExp sigInc tag sigName sig -> [ttypeTerm ty, algTerm alg, TInt ls, TInt ottl, TInt sigExp, TInt sigInc, TInt tag, nameTerm sigName, b64Term sig]
  NSEC nextN covered -> [nameTerm nextN, list (map ttypeTerm covered)]
  DS tag alg digT dig -> [TInt tag, algTerm alg, digestTerm digT, b64Term dig]

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
