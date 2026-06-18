module DNS.RecordSpec (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import DNS.NameSpec (mkName)
import DNS.Record (Type (NSEC))
import qualified DNS.RecordType as RT
import Util.Bytes (Serialize(serialize))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

c :: Char -> Word8
c = toEnum . fromEnum

tests :: TestTree
tests =
  testGroup
    "DNS.Record"
    -- From: https://datatracker.ietf.org/doc/html/rfc4034#section-4.3
    [ testCase "serializes RFC 4034 NSEC RDATA" $
        serialize (NSEC host [RT.A, RT.MX, RT.RRSIG, RT.NSEC])
          @?= BS.pack
            [ 0x04, c 'h', c 'o', c 's', c 't'
            , 0x07, c 'e', c 'x', c 'a', c 'm', c 'p', c 'l', c 'e'
            , 0x03, c 'c', c 'o', c 'm', 0x00
            , 0x00, 0x06, 0x40, 0x01, 0x00, 0x00, 0x00, 0x03
            ]
    ]
  where
    host = mkName ["host", "example", "com", ""]
