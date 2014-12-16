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
import Data.Monoid ((<>))
import Data.Aeson as A (decode, object, Value(..), Object)
import Data.Map.Lazy as Map (fromList)

import qualified Liqpay.Coder as LC
import qualified Liqpay.Client as Client


type PublicKey = Text
type PrivateKey = Text

data Liqpay = Liqpay { getPublicKey  :: PublicKey
                     , getPrivateKey :: PrivateKey
                     , getHost       :: String
                     , getApiUrl     :: String }


type OperationName = String
type Signature = Either Text Text

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
    let encData = LC.encodeParams params'
        signature = cnbSignature params' liqpay
    case signature of
        Left msg         -> return $ toMObject $ A.object [(T.pack "error", String msg)]
        Right signature' -> do
            let encParams = Map.fromList [ (T.pack "data" , encData)
                                         , (T.pack "signature" , signature')
                                         ]
            respBody <- Client.request (getHost liqpay) (getApiUrl liqpay ++ path) encParams
            return $ decode $ BLC.fromStrict . BL.toStrict $ respBody
    where params' = insert (T.pack "public_key") (getPublicKey liqpay) params


cnbForm :: Params -> Liqpay -> Either Text Html
cnbForm params liqpay = do
    let  verifiedParams  = validateParam "version" (Right params)
    case verifiedParams of
        Left msg      -> Left msg
        Right params' -> do
            let language        = findWithDefault (T.pack "ru") (T.pack "language") params'
                params''        = insert (T.pack "public_key") (getPublicKey liqpay) params'
                encData         = LC.encodeParams params''
                signature       = cnbSignature params'' liqpay
            case signature of
                Left msg         -> Left msg
                Right signature' -> Right $ form ! formAttributes << fmap createHiddenInputAttr (toList encParams)
                                                          +++ (input ! imageAttributes)
                    where encParams       = Map.fromList [ (T.pack "data" , encData)
                                                         , (T.pack "signature" , signature')
                                                         ]
                          formAttributes  =
                              [ method "post"
                              , action ("https://" ++ getHost liqpay ++ getApiUrl liqpay ++ "checkout")
                              , strAttr "accept-charset" "utf-8"
                              ]
                          imageAttributes =
                              [ thetype "image"
                              , src ("//static.liqpay.com/button/p1" ++ T.unpack language ++ ".radius.png")
                              , name "btn_text"
                              ]


createHiddenInputAttr :: (AttributeName, AttributeValue) ->  Html
createHiddenInputAttr (n,v) = input ! [ thetype "hidden"
                                      , name (T.unpack n)
                                      , value (T.unpack v)
                                      ]


cnbSignature :: Params -> Liqpay -> Signature
cnbSignature params liqpay = do
    let verifiedParams = validateParam "description" $ validateParam "currency" $ validateParam "amount" $ validateParam "version" (Right params)
    case verifiedParams of
        Left msg      -> Left msg
        Right params' -> do
            let params''    =  insert (T.pack "public_key") (getPublicKey liqpay) params'
                signature  =  getPrivateKey liqpay
                           <> LC.encodeParams params''
                           <> getPrivateKey liqpay
                in Right (signStr signature)


getText :: String -> Params -> Maybe Text
getText s = Map.lookup (T.pack s)

validateParam :: String -> Either Text Params -> Either Text Params
validateParam key params = case params of
    Left msg      -> Left msg
    Right params' -> if getText key params' == Nothing
                    then Left $ T.pack (key ++ " cannot be Nothing")
                    else params

signStr :: Text -> Text
signStr = T.pack . BLC.unpack . LC.encodeSignature

toMObject :: Value -> Maybe Object
toMObject (Object o) = Just o
toMObject _ = Nothing
