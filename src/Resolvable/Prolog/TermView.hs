{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Prolog.TermView
  ( Resolves(..)
  , TermView(..)
  , toView
  , fromView
  , isResolves
  , isNeq ) where

import DNS.Record (Type)
import qualified DNS.RecordType as T
import Resolvable.Prolog.Symbols
import Resolvable.Prolog.Core
import qualified Util.Graph as G

data Resolves = Resolves
  { resolvesNS :: Term
  , resolvesQType :: Term
  , resolvesZone :: Term
  , resolvesQName :: Term
  , resolvesData :: Term
  , resolvesAlias :: Term
  , resolvesVerify :: Term
  , resolvesAD :: Term
  }
  deriving (Eq, Ord)

instance Show Resolves where
  show (Resolves ns qtype zone qname dat alias ver adb) = show $ resolves ns qtype zone qname dat alias ver adb

instance Show (G.Graph Resolves) where
  show po = unlines (map (\(l, r) -> show r ++ " < " ++ show l) (G.toFlatList po))

data TermView = TVList Term Term
              | TVMember Term Term
              | TVNeq Term Term
              | TVNot Term
              | TVMin Term Term Term
              | TVLeq Term Term
              | TVResolves Resolves
              | TVResolvable Term Term Term
              | TVIsName Term
              | TVRRSIGValid Term Term Term Term
              | TVDigestValid Term Term Term
              | TVNSECUnsigned Term Term
              | TVClosestEncloser Term Term Term Term Term
              | TVNSEC3Unsigned Term Term Term
              | TVKeyTag Term Term
              | TVAd Bool
              | TVFunctor String [ Term ]
              | TVEmptyList
              | TVVar String
              | TVAtom String
              | TVString String
              | TVInt Int
              | TVComplex Type
              | TVAData Term
              | TVAAAAData Term
              | TVCNAMEData Term
              | TVDNAMEData Term
              | TVNSData Term
              | TVTXTData Term
              | TVMXData Term Term
              | TVSOAData Term Term Term Term Term Term Term
              | TVDNSKEYData Term Term Term
              | TVRRSIGData Term Term Term Term Term Term Term Term Term
              | TVNSECData Term Term
              | TVNSEC3Data Term Term Term Term Term Term
              | TVNSEC3ParamData Term Term Term Term
              | TVDSData Term Term Term Term
              | TVZONEMDData Term Term Term Term

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
  | n == notSym = case args of
    [ constraint ] -> TVNot constraint
    _ -> undefined
  | n == minSym = case args of
    [ a, b, c ] -> TVMin a b c
    _ -> undefined
  | n == leqSym = case args of
    [ a, b ] -> TVLeq a b
    _ -> undefined
  | n == resolvesSym = case args of
    [ ns, qtype, zone, qname, d, alias, ver, adb ] -> TVResolves $ Resolves ns qtype zone qname d alias ver adb
    _ -> undefined
  | n == resolvableSym = case args of
    [ qtype, qname, ver ] -> TVResolvable qtype qname ver
    _ -> undefined
  | n == isNameSym = case args of
      [ nam ] -> TVIsName nam
      _ -> undefined
  | n == rrsigValidSym = case args of
    [ nam, dat, sig, key ] -> TVRRSIGValid nam dat sig key
    _ -> undefined
  | n == digestValidSym = case args of
    [ nam, dnskeyTerm, dsTerm ] -> TVDigestValid nam dnskeyTerm dsTerm
    _ -> undefined
  | n == nsecUnsignedSym = case args of
    [ nam, nsecDat ] -> TVNSECUnsigned nam nsecDat
    _ -> undefined
  | n == closestEncloserSym = case args of
    [ nam, exNam, exDat, nonExNam, nonExDat ] -> TVClosestEncloser nam exNam exDat nonExNam nonExDat
    _ -> undefined
  | n == nsec3UnsignedSym = case args of
    [ nam, hashedNam, nsecDat ] -> TVNSEC3Unsigned nam hashedNam nsecDat
    _ -> undefined
  | n == keyTagSym = case args of
    [ key, tag ] -> TVKeyTag key tag
    _ -> undefined
  | n == adSym = case map toView args of
    [ TVInt 0 ] -> TVAd False
    [ TVInt 1 ] -> TVAd True
    _ -> undefined
  | n == ttypeSym T.A = case args of
     [ip] -> TVAData ip
     _ -> undefined
  | n == ttypeSym T.AAAA = case args of
     [ip] -> TVAAAAData ip
     _ -> undefined
  | n == ttypeSym T.CNAME = case args of
     [alias] -> TVCNAMEData alias
     _ -> undefined
  | n == ttypeSym T.DNAME = case args of
     [alias] -> TVDNAMEData alias
     _ -> undefined
  | n == ttypeSym T.NS = case args of
     [ns] -> TVNSData ns
     _ -> undefined
  | n == ttypeSym T.TXT = case args of
     [txt] -> TVTXTData txt
     _ -> undefined
  | n == ttypeSym T.MX = case args of
     [p, mxn] -> TVMXData p mxn
     _ -> undefined
  | n == ttypeSym T.SOA = case args of
     [mn, rn, s, rf, rt, e, m] -> TVSOAData mn rn s rf rt e m
     _ -> undefined
  | n == ttypeSym T.DNSKEY = case args of
     [fs, alg, k] -> TVDNSKEYData fs alg k
     _ -> undefined
  | n == ttypeSym T.RRSIG = case args of
     [ty, alg, ls, ottl, sigExp, sigInc, tag, sigName, sig] -> TVRRSIGData ty alg ls ottl sigExp sigInc tag sigName sig
     _ -> undefined
  | n == ttypeSym T.NSEC = case args of
     [nextN, covered] -> TVNSECData nextN covered
     _ -> undefined
  | n == ttypeSym T.NSEC3 = case args of
     [alg, flags, iter, salt, nextHashedOwner, types] -> TVNSEC3Data alg flags iter salt nextHashedOwner types
     _ -> undefined
  | n == ttypeSym T.NSEC3PARAM = case args of
     [alg, flags, iter, salt] -> TVNSEC3ParamData alg flags iter salt
     _ -> undefined
  | n == ttypeSym T.DS = case args of
     [tag, alg, digT, dig] -> TVDSData tag alg digT dig
     _ -> undefined
  | n == ttypeSym T.ZONEMD = case args of
     [serial, scheme, hashAlgorithm, digest] -> TVZONEMDData serial scheme hashAlgorithm digest
     _ -> undefined
  | otherwise = TVFunctor n args
toView TEmptyList = TVEmptyList
toView (TVar v) = TVVar v
toView (TAtom n) = TVAtom n
toView (TString s) = TVString s
toView (TInt i) = TVInt i
toView (TComplex dat) = TVComplex dat

fromView :: TermView -> Term
fromView (TVList a b) = TFunctor listSym [ a, b ]
fromView (TVMember a b) = member a b
fromView (TVNeq a b) = a \= b
fromView (TVNot constraint) = nott constraint
fromView (TVMin a b c) = minTerm a b c
fromView (TVLeq a b) = a =< b
fromView (TVResolves (Resolves ns qtype zone qname d alias ver adb)) = resolves ns qtype zone qname d alias ver adb
fromView (TVResolvable qtype qname ver) = resolvable qtype qname ver
fromView (TVIsName nam) = isName nam
fromView (TVRRSIGValid nam dat sig key) = rrsigValid nam dat sig key
fromView (TVDigestValid nam dnskeyTerm dsTerm) = digestValid nam dnskeyTerm dsTerm
fromView (TVNSECUnsigned nam nsecDat) = nsecUnsigned nam nsecDat
fromView (TVClosestEncloser nam exNam exDat nonExNam nonExDat) = closestEncloser nam exNam exDat nonExNam nonExDat
fromView (TVNSEC3Unsigned nam hashedNam nsecDat) = nsec3Unsigned nam hashedNam nsecDat
fromView (TVKeyTag key tag) = keyTag key tag
fromView (TVAd b) = ad (fromEnum b)
fromView (TVFunctor n args) = TFunctor n args
fromView TVEmptyList = TEmptyList
fromView (TVVar v) = TVar v
fromView (TVAtom n) = TAtom n
fromView (TVString s) = TString s
fromView (TVInt i) = TInt i
fromView (TVComplex dat) = TComplex dat
fromView (TVAData ip) = TFunctor (ttypeSym T.A) [ip]
fromView (TVAAAAData ip) = TFunctor (ttypeSym T.AAAA) [ip]
fromView (TVCNAMEData alias) = TFunctor (ttypeSym T.CNAME) [alias]
fromView (TVDNAMEData alias) = TFunctor (ttypeSym T.DNAME) [alias]
fromView (TVNSData ns) = TFunctor (ttypeSym T.NS) [ns]
fromView (TVTXTData txt) = TFunctor (ttypeSym T.TXT) [txt]
fromView (TVMXData p n) = TFunctor (ttypeSym T.MX) [p, n]
fromView (TVSOAData mn rn s rf rt e m) = TFunctor (ttypeSym T.SOA) [mn, rn, s, rf, rt, e, m]
fromView (TVDNSKEYData fs alg k) = TFunctor (ttypeSym T.DNSKEY) [fs, alg, k]
fromView (TVRRSIGData ty alg ls ottl sigExp sigInc tag sigName sig) = TFunctor (ttypeSym T.RRSIG) [ty, alg, ls, ottl, sigExp, sigInc, tag, sigName, sig]
fromView (TVNSECData nextN covered) = TFunctor (ttypeSym T.NSEC) [nextN, covered]
fromView (TVNSEC3Data alg flags iter salt nextHashedOwner types) = TFunctor (ttypeSym T.NSEC3) [alg, flags, iter, salt, nextHashedOwner, types]
fromView (TVNSEC3ParamData alg flags iter salt) = TFunctor (ttypeSym T.NSEC3PARAM) [alg, flags, iter, salt]
fromView (TVDSData tag alg digT dig) = TFunctor (ttypeSym T.DS) [tag, alg, digT, dig]
fromView (TVZONEMDData serial scheme hashAlgorithm digest) = TFunctor (ttypeSym T.ZONEMD) [serial, scheme, hashAlgorithm, digest]

isResolves :: Term -> Bool
isResolves (toView -> TVResolves {}) = True
isResolves _ = False

isNeq :: Term -> Bool
isNeq (toView -> TVNeq {}) = True
isNeq _ = False
