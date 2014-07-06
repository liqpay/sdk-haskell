module Liqpay.Liqpay
    ( auth
    , api
    , cnbForm
    , cnbSignature
    , signStr
    ) where

import Text.XHtml.Strict
import Data.ByteString.Lazy.Char8 as BLC (unpack, putStr, fromStrict)
import Data.Text as T (Text, pack, unpack)
import Data.Map.Lazy as Map (Map, lookup, insert, findWithDefault, toList)
import Data.Monoid

import Liqpay.Coder as LC
import Liqpay.Client as Client


data Liqpay = Liqpay { publicKey  :: Text
                     , privateKey :: Text
                     , host       :: String
                     , apiUrl     :: String }

type Params = Map.Map Text Text

auth :: (Text,Text) -> Liqpay
auth (public, private) = Liqpay { publicKey  = public
                                , privateKey = private
                                , host       = "www.liqpay.com"
                                , apiUrl     = "/api/" }


api :: String -> Params -> Liqpay -> IO ()
api path params liqpay =
    Client.request (host liqpay) (apiUrl liqpay ++ path) params' >>= BLC.putStr . BLC.fromStrict
    where params' = insert (T.pack "public_key") (publicKey liqpay) params


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
              , strAttr "accept-charset" "utf-8" ]
          imageAttributes = 
              [ thetype "image"
              , src ("//static.liqpay.com/button/p1" ++ T.unpack language ++ ".radius.png")
              , name "btn_text" ]


createHiddenInputAttr :: (Text, Text) ->  Html
createHiddenInputAttr (n,v) = input ! [ thetype "hidden"
                                      , name (T.unpack n)
                                      , value (T.unpack v) ]

     
cnbSignature :: Params -> Liqpay -> String
cnbSignature params liqpay = 
    let signature = T.unpack (privateKey liqpay)
            ++ validateParam "amount" params
            ++ validateParam "currency" params
            ++ T.unpack (publicKey liqpay)
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
