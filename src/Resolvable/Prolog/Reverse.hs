module Resolvable.Prolog.Reverse
  ( asList
  , asName
  , asTType
  , asType
  , asTypes
  , asDNSKEYData
  , asDSData
  , asRRSigData
  , asNSECData
  , asNSEC3Data
  , asRRSetData) where

import Control.Monad.Trans.Except
import Control.Monad ((<=<), when)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Parsing.Data (parseTypeM)
import Data.Char (toUpper)

import Parsing.Tokens (ipv4, ipv6)
import DNS.SEC.Keys (dnsKeyFlagsFromInt)
import DNS.SEC.Crypto (algorithmFromInt, digestFromInt, nsec3DigestFromInt, nsec3FlagsFromInt)
import Util.Monad
import Util.Except
import Util.Parsing (parseS)
import Util.Bytes (fromB64, fromHex)

asList :: Term -> Except String [Term]
asList = exceptMaybe "term is not a list" . collectList

asName :: Term -> Except String (Name String)
asName t = do
  ts <- asList t
  ls <- exceptMaybe ("label not ground: " ++ show t) $ mapM asAtom ts
  -- Add empty label was we do not represent it in Prolog
  return $ Name (NE.reverse $ "" NE.:| ls)
  where
    asAtom :: Term -> Maybe String
    asAtom (TAtom a) = Just a
    asAtom _ = Nothing

asTType :: Term -> Except String T.Type
asTType (TAtom lbl) = parseTypeM $ map toUpper lbl
asTType t = throwE ("data not ground: " ++ show t)

asTTypes :: Term -> ExceptM [T.Type]
asTTypes = toExceptM . (mapM asTType <=< asList)

asType :: Term -> Except String Type
asType (TComplex d) = return d
asType t = throwE ("data not ground: " ++ show t)

asTypes :: Term -> Except String [Type]
asTypes t = do
  l <- exceptMaybe ("data not ground: " ++ show t) $ collectList t
  mapM asType l

asDNSKEYData :: Type -> Except String DNSKEYData
asDNSKEYData (DNSKEY dat) = return dat
asDNSKEYData t = throwE $ "expected DNSKEY got: " ++ show t

asDSData :: Type -> Except String DSData
asDSData (DS dat) = return dat
asDSData t = throwE $ "expected DS got: " ++ show t

asRRSigData :: Type -> Except String (RRSIGData BS.ByteString)
asRRSigData (RRSIG dat) = return dat
asRRSigData t = throwE $ "expected RRSIG got: " ++ show t

asNSECData :: Type -> Except String NSECData
asNSECData (NSEC dat) = return dat
asNSECData t = throwE $ "expected NSEC got: " ++ show t

asNSEC3Data :: Type -> Except String NSEC3Data
asNSEC3Data (NSEC3 dat) = return dat
asNSEC3Data t = throwE $ "expected NSEC3 got: " ++ show t

toViewM :: Monad m => Term -> m TermView
toViewM = return . toView

asRRSetData :: Term -> ExceptM Type
asRRSetData t = case toView t of
  TVAData ip -> do
    TVString ipS <- toViewM ip
    A <$> parseS ipv4 ipS
  TVAAAAData ip -> do
    TVString ipS <- toViewM ip
    AAAA <$> parseS ipv6 ipS
  TVCNAMEData alias -> CNAME <$> toExceptM (asName alias)
  TVDNAMEData alias -> DNAME <$> toExceptM (asName alias)
  TVNSData ns -> NS <$> toExceptM (asName ns)
  TVTXTData txt -> do
    TVString txt' <- toViewM txt
    return $ TXT txt'
  TVMXData p n -> do
    TVInt p' <- toViewM p
    n' <- toExceptM $ asName n
    return $ MX p' n'
  TVSOAData mn rn s rf rt e m -> do
    mn' <- toExceptM $ asName mn
    rn' <- toExceptM $ asName rn
    TVInt s' <- toViewM s
    TVInt rf' <- toViewM rf
    TVInt rt' <- toViewM rt
    TVInt e' <- toViewM e
    TVInt m' <- toViewM m
    return $ SOA mn' rn' s' rf' rt' e' m'
  TVDNSKEYData fs alg k -> do
    TVInt fs' <- toViewM fs
    TVInt algI <- toViewM alg
    Just alg' <- return $ algorithmFromInt algI
    TVString kB64 <- toViewM k
    k' <- fromB64 kB64
    return $ DNSKEY (DNSKEYData (dnsKeyFlagsFromInt fs') alg' k')
  TVRRSIGData ty alg ls ottl sigExp sigInc tag sigName sig -> do
    ty' <- toExceptM $ asTType ty
    TVInt algI <- toViewM alg
    Just alg' <- return $ algorithmFromInt algI
    TVInt ls' <- toViewM ls
    TVInt ottl' <- toViewM ottl
    TVInt sigExp' <- toViewM sigExp
    TVInt sigInc' <- toViewM sigInc
    TVInt tag' <- toViewM tag
    sigName' <- toExceptM $ asName sigName
    TVString sigB64 <- toViewM sig
    sig' <- fromB64 sigB64
    return $ RRSIG (RRSIGData ty' alg' ls' ottl' sigExp' sigInc' tag' sigName' sig')
  TVNSECData nextN covered -> do
    nextN' <- toExceptM $ asName nextN
    covered' <- asTTypes covered
    return $ NSEC (NSECData nextN' covered')
  TVNSEC3Data alg flags iter salt nextHashedOwner types -> do
    TVInt algI <- toViewM alg
    Just alg' <- return $ nsec3DigestFromInt algI
    TVInt flags' <- toViewM flags
    TVInt iter' <- toViewM iter
    when (iter' < 0) (throwE "negative NSEC3 iteration count")
    TVString saltHex <- toViewM salt
    salt' <- fromHex saltHex
    TVString nextHashedOwnerHex <- toViewM nextHashedOwner
    nextHashedOwner' <- fromHex nextHashedOwnerHex
    types' <- asTTypes types
    return $ NSEC3 (NSEC3Data alg' (nsec3FlagsFromInt flags') iter' salt' nextHashedOwner' types')
  TVNSEC3ParamData alg flags iter salt -> do
    TVInt algI <- toViewM alg
    Just alg' <- return $ nsec3DigestFromInt algI
    TVInt flags' <- toViewM flags
    TVInt iter' <- toViewM iter
    when (iter' < 0) (throwE "negative NSEC3 iteration count")
    TVString saltHex <- toViewM salt
    salt' <- fromHex saltHex
    return $ NSEC3PARAM alg' (nsec3FlagsFromInt flags') iter' salt'
  TVDSData tag alg digT dig -> do
    TVInt tag' <- toViewM tag
    TVInt algI <- toViewM alg
    Just alg' <- return $ algorithmFromInt algI
    TVInt digTI <- toViewM digT
    Just digT' <- return $ digestFromInt digTI
    TVString digHex <- toViewM dig
    dig' <- fromHex digHex
    return $ DS (DSData tag' alg' digT' dig')
  TVZONEMDData serial scheme hashAlgorithm digest -> do
    TVInt serial' <- toViewM serial
    TVInt scheme' <- toViewM scheme
    TVInt hashAlgorithm' <- toViewM hashAlgorithm
    TVString digestHex <- toViewM digest
    digest' <- fromHex digestHex
    return $ ZONEMD serial' scheme' hashAlgorithm' digest'
  _ -> throwE "unrecognized term"
