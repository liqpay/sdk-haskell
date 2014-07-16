{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Liqpay.Client
    ( request
    , request'
    ) where

import Data.Map as Map
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Http.Client as HCL
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Char8 as BLC (pack, toStrict)
import OpenSSL (withOpenSSL)
import qualified OpenSSL.Session as SSL

import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy.Internal as BLI (ByteString)

request
  :: (MonadBaseControl IO m,
      MonadIO m,
      MonadThrow m) =>
     String -> String -> Map Text Text -> m BLI.ByteString
request hostname path' params = do
    req <- parseUrl $ "https://" ++ hostname ++ path'
    withManager $ \manager -> do
        let params'  = Map.map TE.encodeUtf8 params
            params'' = Map.mapKeys TE.encodeUtf8 params'
            headers  = requestHeaders req
            req' = req { method         = "POST"
                       , redirectCount  = 0
                       , requestHeaders = ("Accept","text/html"):("Content-type","application/json"):headers
--                       , checkStatus    = \_ _ _ -> Nothing
                       }
            req'' = urlEncodedBody (Map.toList params'') req'
        runResourceT $ do
            res <- httpLbs req'' manager
            return (responseBody res)


request' :: String -> String -> Map Text Text -> IO S.ByteString
request' hostname path' params = withOpenSSL $ do
    let params'  = Map.map TE.encodeUtf8 params
        params'' = Map.mapKeys TE.encodeUtf8 params'
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
    c <- openConnectionSSL ctx (BLC.toStrict (BLC.pack hostname)) 443
    q <- buildRequest $ do
        HCL.http POST $ BLC.toStrict $ BLC.pack path'
        setAccept "text/html"
        setContentType "application/json"
    sendRequest c q (encodedFormBody (Map.toList params''))
    r <- receiveResponse c concatHandler'
    closeConnection c
    return r
