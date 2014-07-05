{-# LANGUAGE OverloadedStrings #-}

module Liqpay.Client (request) where

import Data.Map as Map
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Http.Client
import Data.ByteString.Lazy.Char8 as BLC (pack, toStrict)
import OpenSSL (withOpenSSL)
import qualified OpenSSL.Session as SSL


request :: String -> String -> Map Text Text -> IO ()
request host path params = withOpenSSL $ do
    let params'  = Map.map TE.encodeUtf8 params
        params'' = Map.mapKeys TE.encodeUtf8 params'
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
    c <- openConnectionSSL ctx (BLC.toStrict (BLC.pack host)) 443
    q <- buildRequest $ do
        http POST $ BLC.toStrict $ BLC.pack path
        setAccept "text/html"
        setContentType "application/json"
    sendRequest c q (encodedFormBody (Map.toList params''))
    receiveResponse c debugHandler
    closeConnection c
