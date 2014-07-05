module Liqpay.Liqpay
    ( auth
    , api
    , cnbForm
    , cnbSignature
    , signStr
    ) where

import Text.XHtml.Strict
import Data.ByteString.Lazy.Char8 as BLC (unpack)
import Data.Text as T (Text, pack, unpack)
import Data.Map.Lazy as Map (Map, lookup, insert, findWithDefault, toList)

import Liqpay.Coder as LC
import Liqpay.Client as Client


data Liqpay = Liqpay { publicKey  :: Text
                     , privateKey :: Text
                     , host        :: String
                     , apiUrl      :: String }

type Params = Map.Map Text Text


auth :: (Text,Text) -> Liqpay
auth (public, private) = Liqpay { publicKey  = public
                                , privateKey = private
                                , host        = "www.liqpay.com"
                                , apiUrl      = "/api/"}


api :: String -> Params -> Liqpay -> IO ()
api path params liqpay =
    Client.request (host liqpay) (apiUrl liqpay ++ path) params'
    where params'     = insert (T.pack "public_key") (publicKey liqpay) params


cnbForm :: Params -> Liqpay -> Html
cnbForm params liqpay = form ! formAttributes
    << fmap createHiddenInputAttr (toList params'') 
        +++ (input ! imageAttributes)
    where language        = findWithDefault (T.pack "ru") (T.pack "language") params
          signature       = T.pack $ cnbSignature params liqpay
          params'         = insert (T.pack "public_key") (publicKey liqpay) params
          params''        = insert (T.pack "signature") signature params'
          formAttributes  = 
              [ method "post"
              , action ("https://" ++ host liqpay ++ apiUrl liqpay ++ "pay")
              , strAttr "accept-charset" "utf-8"]
          imageAttributes = 
              [ thetype "image"
              , src ("//static.liqpay.com/button/p1" ++ T.unpack language ++ ".radius.png")
              , name "btn_text"]


createHiddenInputAttr :: (Text, Text) ->  Html
createHiddenInputAttr (n,v) = input ! [ thetype "hidden"
                                      , name (T.unpack n)
                                      , value (T.unpack v) ]

     
cnbSignature :: Params -> Liqpay -> String
cnbSignature params liqpay = 
    let signature   = T.unpack (privateKey liqpay)
            ++ maybe (error "amount cannot be Nothing") T.unpack (getText "amount" params)
            ++ maybe (error "currency cannot be Nothing") T.unpack (getText "currency" params)
            ++ T.unpack (publicKey liqpay)
            ++ maybe "" T.unpack (getText "order_id" params)
            ++ maybe "" T.unpack (getText "type" params)
            ++ maybe (error "description cannot be Nothing") T.unpack (getText "description" params)
            ++ maybe "" T.unpack (getText "result_url" params)
            ++ maybe "" T.unpack (getText "server_url" params)
            ++ maybe "" T.unpack (getText "sender_first_name" params)
            ++ maybe "" T.unpack (getText "sender_last_name" params)
            ++ maybe "" T.unpack (getText "sender_middle_name" params)
            ++ maybe "" T.unpack (getText "sender_country" params)
            ++ maybe "" T.unpack (getText "sender_city" params)
            ++ maybe "" T.unpack (getText "sender_address" params)
            ++ maybe "" T.unpack (getText "sender_postal_code" params) 
        in signStr signature

getText :: String -> Params -> Maybe Text
getText s = Map.lookup (T.pack s)

signStr :: String -> String
signStr = BLC.unpack . LC.encodeSignature
