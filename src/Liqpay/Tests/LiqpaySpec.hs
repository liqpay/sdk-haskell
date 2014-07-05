{-# LANGUAGE OverloadedStrings #-}
module Liqpay.Tests.LiqpaySpec where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Map.Lazy as Map
import Data.Text as T

import Text.XHtml.Strict (showHtmlFragment)

import Liqpay.Liqpay

tests :: IO ()
tests = hspec $ do
    describe "Liqpay.signStr" $ do
        it "returns encoded String" $ do
            signStr "test" `shouldBe` "qUqP5cyxm6YcTAhz05Hph5gvu9M="
    describe "Liqpay.cnbSignature" $ do
        it "returns valid signature with Text values" $ do
            let params = [(T.pack "amount",T.pack "1.2"),(T.pack "currency",T.pack "USD"),(T.pack "description",T.pack "my comment")]
                liqpay = auth (T.pack "public_key", T.pack "private_key")
                in cnbSignature (Map.fromList params) liqpay `shouldBe` "Bi2FxqSmM2A5ZFt5l397f/QSyQM="
        it "returns valid signature with overloaded String values" $ do
            let params = [("amount","1.2"),("currency","USD"),("description","my comment")]
                liqpay = auth (T.pack "public_key", T.pack "private_key")
                in cnbSignature (Map.fromList params) liqpay `shouldBe` "Bi2FxqSmM2A5ZFt5l397f/QSyQM="
--        it "returns valid signature with overloaded String values and cyrillic symbols" $ do
--            let params = [("amount","1.2"),("currency","USD"),("description","коментарий")]
--                liqpay = auth (T.pack "public_key", T.pack "private_key")
--                in cnbSignature (Map.fromList params) liqpay `shouldBe` "Bi2FxqSmM2A5ZFt5l397f/QSyQM="
        it "returns error without amount" $ do
            let liqpay = auth (T.pack "public_key", T.pack "private_key")
                in evaluate (cnbSignature (Map.fromList ([("currency","USD"),("description","my comment")]) ) liqpay) `shouldThrow` errorCall "amount cannot be Nothing"
        it "returns error without currency" $ do
            let liqpay = auth (T.pack "public_key", T.pack "private_key")
                in evaluate (cnbSignature (Map.fromList ([("amount","1.2"),("description","my comment")]) ) liqpay)  `shouldThrow` errorCall "currency cannot be Nothing"
        it "returns error without description" $ do
            let liqpay = auth (T.pack "public_key", T.pack "private_key")
                in evaluate (cnbSignature (Map.fromList ([("amount","1.2"),("currency","USD")]) ) liqpay) `shouldThrow` errorCall "description cannot be Nothing"
    describe "Liqpay.cnbForm" $ do
        it "returns post form" $ do
            let liqpay = auth (T.pack "public_key", T.pack "private_key")
                in showHtmlFragment (cnbForm (Map.fromList([(T.pack "amount",T.pack "1.2"),(T.pack "currency",T.pack "USD"),(T.pack "description",T.pack "my comment"),(T.pack "language", T.pack "en")])) liqpay) `shouldBe` "<form method=\"post\" action=\"https://www.liqpay.com/api/pay\" accept-charset=\"utf-8\"><input type=\"hidden\" name=\"amount\" value=\"1.2\" /><input type=\"hidden\" name=\"currency\" value=\"USD\" /><input type=\"hidden\" name=\"description\" value=\"my comment\" /><input type=\"hidden\" name=\"language\" value=\"en\" /><input type=\"hidden\" name=\"public_key\" value=\"public_key\" /><input type=\"hidden\" name=\"signature\" value=\"Bi2FxqSmM2A5ZFt5l397f/QSyQM=\" /></form><input type=\"image\" src=\"//static.liqpay.com/button/p1en.radius.png\" name=\"btn_text\" />"
