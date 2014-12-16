{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Liqpay.Client
    ( request
    ) where

import Data.Map as Map
import Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy.Internal as BLI (ByteString)

import Data.Aeson (encode)

request :: ( MonadBaseControl IO m
           , MonadIO m
           , MonadThrow m
           )
        => String
        -> String
        -> Map Text Text
        -> m BLI.ByteString
request hostname path' params = do
    req <- parseUrl $ "https://" ++ hostname ++ path'
    withManager $ \manager -> do
        let params'  = Map.map TE.encodeUtf8 params
            params'' = Map.mapKeys TE.encodeUtf8 params'
            headers  = requestHeaders req
            req'     = req { requestBody = RequestBodyLBS $ encode params
                           , method         = "POST"
                           , redirectCount  = 0
                           , requestHeaders = ("Accept","text/html"):headers
--                         , checkStatus    = \_ _ _ -> Nothing
                           }
            req''    = urlEncodedBody (Map.toList params'') req'
        runResourceT $ do
            res <- httpLbs req'' manager
            return $ responseBody res
