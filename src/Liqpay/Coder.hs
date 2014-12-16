module Liqpay.Coder ( encodeSignature
                    , encodeParams
                    ) where

import Data.Digest.Pure.SHA as S (sha1, bytestringDigest)
import Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Base64.Lazy as B (encode)
import Data.ByteString.Lazy.Char8 as BLC (pack, unpack)
import Data.Map.Lazy (Map)
import Data.Text as T (Text, pack, unpack)
import Data.Aeson as A (encode)

encodeSignature :: Text -> L.ByteString
encodeSignature = B.encode . getSha1 . T.unpack

getSha1 :: String -> L.ByteString
getSha1 a = S.bytestringDigest . S.sha1 $ BLC.pack a

encodeParams :: Map Text Text -> Text
encodeParams params = T.pack $ BLC.unpack $ B.encode $ A.encode params
