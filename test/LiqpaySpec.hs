{-# LANGUAGE OverloadedStrings #-}
module LiqpaySpec where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Map.Lazy as Map
import Data.Text as T
import Data.Aeson as A (decode, Object)

import Liqpay.Liqpay

tests :: IO ()
tests = hspec $ do
    describe "Liqpay.signStr" $ do
        it "returns encoded Text" $ do
            signStr "test" `shouldBe` "qUqP5cyxm6YcTAhz05Hph5gvu9M="
        it "returns encoded Text if signature includes cyrillic symbols" $ do
            signStr "тест" `shouldBe` "ju9KzLCkWKVzZrdRjm2jKL9Wgo4="

    describe "Liqpay.cnbSignature" $ do
        it "returns error without version" $ do
            let liqpay = auth ("public_key", "private_key")
                signature = cnbSignature Nothing (Map.fromList [])
                in evaluate (signature liqpay) `shouldReturn` Left "version cannot be Nothing"
        it "returns error without amount" $ do
            let liqpay = auth ("public_key", "private_key")
                signature = cnbSignature Nothing (Map.fromList [("version","3")])
                in evaluate (signature liqpay) `shouldReturn` Left "amount cannot be Nothing"
        it "returns error without currency" $ do
            let liqpay = auth ("public_key", "private_key")
                signature = cnbSignature Nothing (Map.fromList [("version","3"),("amount","0")])
                in evaluate (signature liqpay) `shouldReturn` Left "currency cannot be Nothing"
        it "returns error without description" $ do
            let liqpay = auth ("public_key", "private_key")
                signature = cnbSignature Nothing (Map.fromList [("version","3"),("amount","0"),("currency","UAH")])
                in evaluate (signature liqpay) `shouldReturn` Left "description cannot be Nothing"
        it "returns valid signature if all params are present" $ do
            let liqpay = auth ("public_key", "private_key")
                signature = cnbSignature Nothing (Map.fromList [("version","3"),("amount","0"),("currency","UAH"),("description","my comment")])
                in evaluate (signature liqpay) `shouldReturn` Right "3B1QfGVzwj/VthFyuXf29kuomxU="
--                in evaluate (signature liqpay) `shouldReturn` Right "D6rMq11PauA5L4ZWIIF/8d/RDoI="
        it "returns valid signature if cyrillic symbols are present" $ do
            let liqpay = auth ("public_key", "private_key")
                signature = cnbSignature Nothing (Map.fromList [("version","3"),("amount","0"),("currency","UAH"),("description","мой комментарий")])
                in evaluate (signature liqpay) `shouldReturn` Right "oB03d0G62VZLBNCxgWsSZS+sV1s="
--                in evaluate (signature liqpay) `shouldReturn` Right "JDHCPlnRhBj5mf/biMw0cXbxXu4="

    describe "Liqpay.cnbForm" $ do
        it "returns error without version" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList []
                form = cnbForm params liqpay
                html = case form of
                    Left msg -> msg
                    Right html' -> pack $ show html'
                in evaluate html `shouldReturn` "version cannot be Nothing"
        it "returns valid html form if version is present" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList [ ("version","3")
                                      , ("amount","1")
                                      , ("currency","UAH")
                                      , ("description","my comment")
                                      ]
                form = cnbForm params liqpay
                html = case form of
                    Left msg -> msg
                    Right html' -> pack $ show html'
                in evaluate html `shouldReturn` "<form method=\"post\" action=\"https://www.liqpay.ua/api/3/checkout\" accept-charset=\"utf-8\"\n><input type=\"hidden\" name=\"data\" value=\"eyJhbW91bnQiOiIxIiwicHVibGljX2tleSI6InB1YmxpY19rZXkiLCJjdXJyZW5jeSI6IlVBSCIsInZlcnNpb24iOiIzIiwiZGVzY3JpcHRpb24iOiJteSBjb21tZW50In0=\"\n   /><input type=\"hidden\" name=\"signature\" value=\"QfUGmGtN0Fg7KWhYHSiWfpWcpHw=\"\n   /></form\n><input type=\"image\" src=\"//static.liqpay.ua/button/p1ru.radius.png\" name=\"btn_text\"\n />"

    describe "Liqpay.api" $ do
        it "returns error without version" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList []
            api "payment/pay" params liqpay `shouldReturn` (decode "{\"error\":\"version cannot be Nothing\"}" :: Maybe Object)
{-
        it "returns error without amount" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList [("version","3")]
            api "payment/pay" params liqpay `shouldReturn` (decode "{\"result\":\"err_access\",\"code\":\"err_access\"}" :: Maybe Object)
        it "returns err_access with valid params and signature build in pay request" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList [("version","3"),("amount","1"),("currency","UAH"),("description","my comment")]
            api "payment/pay" params liqpay `shouldReturn` (decode "{\"result\":\"err_access\",\"code\":\"err_access\"}" :: Maybe Object)
        it "returns payment_not_found with valid params and signature build in status request" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList [("version","3"),("amount","1"),("currency","UAH"),("description","my comment")]
            api "payment/status" params liqpay `shouldReturn` (decode "{\"result\":\"error\",\"description\":\"payment_not_found\"}" :: Maybe Object)
        it "returns payment_not_found with valid params and signature build in status request when cyrillic symbols exists" $ do
            let liqpay = auth ("public_key", "private_key")
                params = Map.fromList [("version","3"),("amount","1"),("currency","UAH"),("description","мой комментарий")]
            api "payment/status" params liqpay `shouldReturn` (decode "{\"result\":\"error\",\"description\":\"payment_not_found\"}" :: Maybe Object)
-}
