module Liqpay.Coder ( encodeSignature) where

import Data.Digest.Pure.SHA as S (sha1, bytestringDigest)
import Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Base64.Lazy as B (encode)
import Data.ByteString.Lazy.Char8 as LC (pack)

encodeSignature :: String -> L.ByteString
encodeSignature = B.encode . getSha1

getSha1 :: String -> L.ByteString
getSha1 a = S.bytestringDigest . S.sha1 $ LC.pack a
