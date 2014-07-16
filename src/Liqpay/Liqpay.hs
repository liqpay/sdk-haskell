module Liqpay.Liqpay
    ( auth
    , api
    , cnbForm
    , cnbSignature
    , signStr
    ) where

import Text.XHtml.Strict
import Data.ByteString.Lazy.Char8 as BLC (unpack, fromStrict)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Text as T (Text, pack, unpack)
import Data.Map.Lazy as Map (Map, lookup, insert, findWithDefault, toList)
import Data.Monoid
import Data.Aeson (decode, Object)

import qualified Liqpay.Coder as LC
import qualified Liqpay.Client as Client

type PublicKey = Text
type PrivateKey = Text

data Liqpay = Liqpay { getPublicKey  :: PublicKey
                     , getPrivateKey :: PrivateKey
                     , getHost       :: String
                     , getApiUrl     :: String }


type OperationName = String
type Signature = String

type ParamName = Text
type ParamValue = Text

type Params = Map.Map ParamName ParamValue

type AttributeName = Text
type AttributeValue = Text

type ApiResponse = Maybe Object

auth :: (PublicKey, PrivateKey) -> Liqpay
auth (public, private) = Liqpay { getPublicKey  = public
                                , getPrivateKey = private
                                , getHost       = "www.liqpay.com"
                                , getApiUrl     = "/api/" }


api :: OperationName -> Params -> Liqpay -> IO ApiResponse
api path params liqpay = do
--    respBody <- Client.request' (getHost liqpay) (getApiUrl liqpay ++ path) params'
--    return $ decode $ BLC.fromStrict $ respBody
    respBody <- Client.request (getHost liqpay) (getApiUrl liqpay ++ path) params' 
    return $ decode $ BLC.fromStrict . BL.toStrict $ respBody
    where params' = insert (T.pack "public_key") (getPublicKey liqpay) params


cnbForm :: Params -> Liqpay -> Html
cnbForm params liqpay = form ! formAttributes
    << fmap createHiddenInputAttr (toList params'') 
        +++ (input ! imageAttributes)
    where language        = findWithDefault (T.pack "ru") (T.pack "language") params
          signature       = T.pack $ cnbSignature params liqpay
          params'         = insert (T.pack "public_key") (getPublicKey liqpay) params
          params''        = insert (T.pack "signature") signature params'
          formAttributes  = 
              [ method "post"
              , action ("https://" ++ getHost liqpay ++ getApiUrl liqpay ++ "pay")
              , strAttr "accept-charset" "utf-8" ]
          imageAttributes = 
              [ thetype "image"
              , src ("//static.liqpay.com/button/p1" ++ T.unpack language ++ ".radius.png")
              , name "btn_text" ]


createHiddenInputAttr :: (AttributeName, AttributeValue) ->  Html
createHiddenInputAttr (n,v) = input ! [ thetype "hidden"
                                      , name (T.unpack n)
                                      , value (T.unpack v) ]

     
cnbSignature :: Params -> Liqpay -> Signature
cnbSignature params liqpay = 
    let signature = T.unpack (getPrivateKey liqpay)
            ++ validateParam "amount" params
            ++ validateParam "currency" params
            ++ T.unpack (getPublicKey liqpay)
            ++ maybe [] T.unpack (mconcat [ getText "order_id" params
                                          , getText "type" params ])
            ++ validateParam "description" params
            ++ maybe [] T.unpack (mconcat [ getText "result_url" params
                                          , getText "server_url" params
                                          , getText "sender_first_name" params
                                          , getText "sender_last_name" params
                                          , getText "sender_middle_name" params
                                          , getText "sender_country" params
                                          , getText "sender_city" params
                                          , getText "sender_address" params
                                          , getText "sender_postal_code" params ])
        in signStr signature

getText :: String -> Params -> Maybe Text
getText s = Map.lookup (T.pack s)

validateParam :: String -> Params -> String
validateParam key params = maybe (error (key ++ " cannot be Nothing")) T.unpack (getText key params)

signStr :: String -> String
signStr = BLC.unpack . LC.encodeSignature
