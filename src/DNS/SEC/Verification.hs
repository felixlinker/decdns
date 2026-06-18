{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.Verification
  ( verify ) where

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import DNS.Name
import qualified DNS.RecordType as T
import DNS.Record
import Util.Bytes
import DNS.SEC.Crypto
import DNS.RRSet
import Data.Maybe (fromMaybe)

verify :: StdRRSet -> T.Type -> AlgorithmType -> Int -> Int -> Int -> Int -> Int -> Name String -> BS.ByteString -> BS.ByteString -> Bool
verify (canonicalRRSet -> rrset) cov alg lc ttl sigExp sigNbf tag (canonicalName -> sigName) sig key = fromMaybe False $
  case alg of
    ECDSAP256SHA256 -> do
      cSig <- ecdsaSig sig
      cPk <- ecdsaPk key
      return $ ECDSA.verify Hash.SHA256 cPk cSig (BS.concat (rrsigData : serializedRRSet))
    _ -> Nothing
  where
    originalOwner :: Name String
    originalOwner = case name rrset of
      n@(Name ls) ->
        let lc' = labelCount n
        in  if lc' > lc then Name ("*" NE.:| NE.drop (lc' - lc) ls) else n

    -- Set construction for sorting and deduplication
    serializedRRSet :: [BS.ByteString]
    serializedRRSet = S.toAscList $ S.fromList $ NE.toList
      $ fmap serialize $ toRecords $ Named originalOwner (value rrset)

    rrsigData :: BS.ByteString
    rrsigData = serializeRRSIGRData cov alg lc ttl sigExp sigNbf tag sigName
