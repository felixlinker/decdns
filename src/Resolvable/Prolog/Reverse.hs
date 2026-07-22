module Resolvable.Prolog.Reverse
  ( asName
  , asTType
  , asType
  , asTypes
  , asDNSKEYData
  , asDSData
  , asRRSigData
  , asNSECData
  , asNSEC3Data) where

import Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import Resolvable.Prolog.Core
import Util.Monad
import Parsing.Data (parseTypeM)
import Data.Char (toUpper)

asName :: Term -> Except String (Name String)
asName t = do
  ts <- exceptMaybe ("name term is not a list: " ++ show t) $ collectList t
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

