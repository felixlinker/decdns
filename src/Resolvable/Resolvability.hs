{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , fromZone
  , NameServersConfig
  , compileRRSet
  , compileConfig
  , plResolvable ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import NameConstraint
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.Zone (collectGlue)
import DNS.ZoneFile

import Util.Fresh (Fresh)
import Util.Monad

injectLast :: Term -> Term -> Term
injectLast newLast t = case toView t of
  v@(TVList _ _ ) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec TVEmptyList = tList newLast emptyList
    rec (TVList a b) = tList a (rec (toView b))
    -- x cannot be a list
    rec x = tList newLast (fromView x)

captureListTail :: Term -> Term -> Term
captureListTail captureWith t = case toView t of
  v@(TVList _ _) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec (TVList a b) = tList a (rec (toView b))
    rec _ = captureWith

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: ConstrainedRRSet }
  deriving (Eq, Ord)

synthesisCases :: [ConstrainedName -> Fresh (Term, [Term])]
synthesisCases = [nameWithoutSynthesis', nameWithSynthesis]
  where
    nameWithoutSynthesis' :: ConstrainedName -> Fresh (Term, [Term])
    nameWithoutSynthesis' n = return (nameWithoutSynthesis n, [])

resolvableBase :: [Name String] -> Name String -> Term -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
resolvableBase rootServers ns zone s@(Named n rds@((RData _ _ ty) NE.:| _)) =
  foreachM synthesisCases $ \synth ->
  foreachM rootKeyCases   $ \(ver, adb) -> do
    (n', cs) <- synth n
    return $ S.singleton $ Rule rann (resolves (nameTerm ns) (typeTerm ty) zone n' (rrsetTerm rds) bot ver adb) cs
    where
      -- TODO: Refine so that only root keys are trusted
      rootKeyCases :: [(Term, Term)]
      rootKeyCases
        -- Set the ad bit of DNSKEY entries stored at root servers to 1
        | toType ty == T.DNSKEY && ns `elem` rootServers = [(bot, ad 0), (top, ad 1)]
        | otherwise = [(bot, ad 0)]

      rann :: Maybe RComp
      rann = Just (RComp ns s)

resolvableStep :: [Name String] -> Name String -> Term -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
resolvableStep (map nameTerm -> rootServers) ns zone s@(Named n rs) =
  foreachM (NE.toList rs) (\r -> foreachM rootServers (`go` rtype r))
  where
    rannot :: Maybe RComp
    rannot = Just $ RComp ns s

    nameServer :: Term
    nameServer = nameTerm ns

    ownerNoSynth :: Term
    ownerNoSynth = nameWithoutSynthesis n

    go :: Term -> Type -> Fresh (S.Set (Rule (Maybe RComp)))
    go rootServer r = case r of
      NS s' ->
        let delegatedNs = nameTerm s'
            expectGlue = isSubDomain s' (toStringName n)
            glueS = if expectGlue then nameServer else rootServer
            glueRec = if expectGlue then bot else top
        in if nameServer == delegatedNs
          then return S.empty
          else mconcat <$> sequence
            [ foreachM [aType, aaaaType] $ \glueType -> do  -- Unauthenticated
                (nTail, t, d, dIp, rec) <- freshVars5 ("N", "T", "D", "D_IP", "R")
                glueZ <- if expectGlue then return glueZone else wildcard
                -- Do not expect wildcard neq constraints on NS records
                let n' = captureListTail nTail ownerNoSynth
                return (S.singleton $ Rule rannot (resolves nameServer t zone n' d top bot (ad 0))
                  [ resolves delegatedNs t ownerNoSynth n' d rec bot (ad 0)
                  , resolves glueS glueType glueZ delegatedNs dIp glueRec bot (ad 0) ])
            , do  -- NSEC for unsigned subzone
                (nTail, t, d, dNSEC, rec) <- freshVars5 ("N", "T", "D", "D_NSEC", "R")
                let n' = captureListTail nTail ownerNoSynth
                return $ S.singleton $ Rule rannot (resolves nameServer t zone n' d top top (ad 0))
                  [ t \= dsType
                  , resolves nameServer nsecType zone ownerNoSynth (list [dNSEC]) bot top (ad 1)
                  , nsecUnsigned ownerNoSynth dNSEC
                  , resolves nameServer t zone n' d rec bot (ad 0) ]
            , do  -- NSEC3 for owner for unsigned subzone
                (nTail, t, d, nNSEC, dNSEC, rec) <- freshVars6 ("N", "T", "D", "N_NSEC", "D_NSEC", "R")
                let n'' = captureListTail nTail ownerNoSynth
                return $ S.singleton $ Rule rannot (resolves nameServer t zone n'' d top top (ad 0))
                  [ t \= dsType
                  , resolves nameServer nsec3Type zone nNSEC (list [dNSEC]) bot top (ad 1)
                  , nsec3Unsigned ownerNoSynth nNSEC dNSEC
                  , resolves nameServer t zone n'' d rec bot (ad 0) ]
            , do  -- NSEC3 closest encloser proof for unsigned subzone
              (nTail, t, d, nNSEC, nNSEC', dNSEC, dNSEC', rec) <- freshVars8 ("N", "T", "D", "N_NSEC", "N_NSEC", "D_NSEC", "D_NSEC", "R")
              let n'' = captureListTail nTail ownerNoSynth
              return $ S.singleton $ Rule rannot (resolves nameServer t zone n'' d top top (ad 0))
                [ t \= dsType
                , resolves nameServer nsec3Type zone nNSEC (list [dNSEC]) bot top (ad 1)
                , resolves nameServer nsec3Type zone nNSEC' (list [dNSEC']) bot top (ad 1)
                , closestEncloser ownerNoSynth nNSEC dNSEC nNSEC' dNSEC'
                , resolves nameServer t zone n'' d rec bot (ad 0) ]
            , do  -- Authenticated not-DS
                (nTail, t, d, rec) <- freshVars4 ("N", "T", "D", "R")
                let n' = captureListTail nTail ownerNoSynth
                return (S.singleton $ Rule rannot (resolves nameServer t zone n' d top top (ad 1))
                  [ t \= dsType
                  , resolves delegatedNs t ownerNoSynth n' d rec top (ad 1) ])
            , do  -- Authenticated DS
                (l, nTail, d, rec) <- freshVars4 ("L", "N", "D", "R")
                let n' = injectLast l $ captureListTail nTail ownerNoSynth
                return (S.singleton $ Rule rannot (resolves nameServer dsType zone n' d top top (ad 1))
                  [ resolves delegatedNs dsType ownerNoSynth n' d rec top (ad 1) ])
            ]
      CNAME (nameTerm -> cname) -> foreachM synthesisCases $ \synth -> S.fromList <$> sequence
        [ do  -- Unauthenticated
          (n', cs) <- synth n
          (t, d, rec) <- freshVars3 ("T", "D", "R")
          targetZone <- wildcard
          return $ Rule rannot (resolves nameServer t zone n' d top bot (ad 0))
            (cs ++  [ t \= cnameType
                          , resolves rootServer t targetZone cname d rec bot (ad 0) ])
        , do  -- Authenticated
          (n', cs) <- synth n
          (t, d, rec1, rec2, a) <- freshVars5 ("T", "D", "R", "R", "A")
          targetZone <- wildcard
          return $ Rule rannot (resolves nameServer t zone n' d top top a)
            (cs ++  [ t \= cnameType
                          , resolves nameServer cnameType zone ownerNoSynth (list [TComplex r]) rec1 top (ad 1)
                          , resolves rootServer t targetZone cname d rec2 top a ]) ]
      DNAME (nameTerm -> dname) -> S.fromList <$> sequence
        [ do  -- Unauthenticated
          (l, nTail, t, d, rec) <- freshVars5 ("L", "N", "T", "D", "R")
          targetZone <- wildcard
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          return $ Rule rannot (resolves nameServer t zone (rewrite ownerNoSynth) d top bot (ad 0))
            [ resolves rootServer t targetZone (rewrite dname) d rec bot (ad 0) ]
        , do  -- Authenticated
          (l, nTail, t, d, rec1, rec2, a) <- freshVars7 ("L", "N", "T", "D", "R", "R", "A")
          targetZone <- wildcard
          let rewrite = injectLast l . captureListTail nTail
          -- Do not expect wildcard neq constraints on DNAME records
          return $ Rule rannot (resolves nameServer t zone (rewrite ownerNoSynth) d top top a)
            [ resolves nameServer dnameType zone ownerNoSynth (list [TComplex r]) rec1 top (ad 1)
            , resolves rootServer t targetZone (rewrite dname) d rec2 top a ] ]
      -- TODO: Check that sigName is the name of the zone containing the record;
      -- does that mean there ex. a corresponding SOA record? Could be, but seems
      -- technically wrong as there could be intermediate SOA records. It could be
      -- that this will be implicitly addressed by the ad(1) resolution of the
      -- DNSKEY record.
      sig@(RRSIG (RRSIGData cov _ _ _ _ _ _ (nameTerm -> sigName) _)) ->
        case cov of
          T.DNSKEY -> do
            (d, d_DS, k, kRef, rec) <- freshVars5 ("D", "D_DS", "K", "K_REF", "R")
            parentZone <- wildcard
            return $ S.singleton $ Rule rannot
              (resolves nameServer dnskeyType zone ownerNoSynth d bot top (ad 1))
              [ resolves nameServer dnskeyType zone ownerNoSynth d bot bot (ad 0)
                    , resolves rootServer dsType parentZone ownerNoSynth d_DS rec top (ad 1)
                    , member k d
                    , member kRef d_DS
                    , digestValid ownerNoSynth k kRef
                    , rrsigValid ownerNoSynth d (TComplex sig) k ]
          _ -> foreachM synthesisCases $ \synth -> do
            (n', cs) <- synth n
            (d, d_KEY, k) <- freshVars3 ("D", "D_KEY", "K")
            return $ S.singleton $ Rule rannot
              (resolves nameServer (ttypeTerm cov) zone n' d bot top (ad 1))
              (cs ++  [ resolves nameServer (ttypeTerm cov) zone n' d bot bot (ad 0)
                            , resolves nameServer dnskeyType zone sigName d_KEY bot top (ad 1)
                            , member k d_KEY
                            , rrsigValid n' d (TComplex sig) k ])
      A {} -> return S.empty
      AAAA {} -> return S.empty
      TXT {} -> return S.empty
      MX {} -> return S.empty
      SOA {} -> return S.empty
      NSEC {} -> return S.empty
      NSEC3 {} -> return S.empty
      NSEC3PARAM {} -> return S.empty
      DS {} -> return S.empty
      DNSKEY {} -> return S.empty

resolvableR :: Fresh (Rule (Maybe RComp))
resolvableR = do
  (s, q, n, rec, v, a) <- freshVars6 ("S", "Q", "N", "R", "V", "A")
  wc1 <- wildcard
  wc2 <- wildcard
  return $ Rule Nothing (resolvable s q n v a) [ resolves s q wc1 n wc2 rec v a ]

allResolvable :: Rule (Maybe RComp)
allResolvable =
  Rule Nothing (TFunctor "all_resolvable" [ q, rns, ns ]) [ bagof n (resolvable rns q n v a) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"
    v = TVar "Ver"
    a = TVar "Auth"

    bagof :: Term -> Term -> Term -> Term
    bagof x y z = TFunctor "bagof" [ x, y, z ]

compileRRSet :: [Name String] -> Name String -> Term -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
compileRRSet rootServers ns zone rs = (<>) <$> resolvableBase rootServers ns zone rs <*> resolvableStep rootServers ns zone rs

fromZone :: [Name String] -> Name String -> Zone ConstrainedLabel -> Fresh (S.Set (Rule (Maybe RComp)))
fromZone rootServers ns z = do
    let (z', glue) = collectGlue z
    authoritative <- mapM (comp (nameWithoutSynthesis $ zoneName z')) (allRRSets z')
    nonAuthoritative <-  mapM (comp glueZone) glue
    return $ mconcat (authoritative ++ nonAuthoritative)
  where
    comp :: Term -> ConstrainedRRSet -> Fresh (S.Set (Rule (Maybe RComp)))
    comp = compileRRSet rootServers ns

type NameServersConfig = [(Name String, Zone ConstrainedLabel)]

compileConfig :: [Name String] -> NameServersConfig -> Fresh (S.Set (Rule (Maybe RComp)))
compileConfig rootServers = (mconcat <$>) . mapM (uncurry (fromZone rootServers))

plResolvable :: S.Set (Rule (Maybe RComp)) -> Fresh (S.Set (Rule (Maybe RComp)))
plResolvable compiled = do
  res <- resolvableR
  return $ foldr S.insert compiled [allResolvable, res]
