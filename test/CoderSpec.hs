{-# LANGUAGE OverloadedStrings #-}
module CoderSpec where

import Test.Hspec
import Data.Map.Lazy as M
import Liqpay.Coder

tests :: IO ()
tests = hspec $ do
    describe "Coder.encodeSignature" $ do
        it "returns encoded ByteString" $ do
            encodeSignature "test" `shouldBe` "qUqP5cyxm6YcTAhz05Hph5gvu9M="
        it "returns encoded ByteString when signature includes cyrillic symbols" $ do
            encodeSignature "тест" `shouldBe` "ju9KzLCkWKVzZrdRjm2jKL9Wgo4="
    describe "Coder.encodeParams" $ do
        it "returns encoded Text" $ do
            let params = M.fromList [ ("version","3")
                                    , ("amount","0.00")
                                    , ("currency","UAH")
                                    , ("description","my comment")
                                    , ("language", "ru")
                                    , ("sandbox","1")
                                    ]
                in encodeParams params `shouldBe` "eyJhbW91bnQiOiIwLjAwIiwic2FuZGJveCI6IjEiLCJjdXJyZW5jeSI6IlVBSCIsInZlcnNpb24iOiIzIiwibGFuZ3VhZ2UiOiJydSIsImRlc2NyaXB0aW9uIjoibXkgY29tbWVudCJ9"
        it "returns encoded Text when signature includes cyrillic symbols" $ do
            let params = M.fromList [ ("version","3")
                                    , ("amount","0.00")
                                    , ("currency","UAH")
                                    , ("description","мой комментарий")
                                    , ("language", "ru")
                                    , ("sandbox","1")
                                    ]
                in encodeParams params `shouldBe` "eyJhbW91bnQiOiIwLjAwIiwic2FuZGJveCI6IjEiLCJjdXJyZW5jeSI6IlVBSCIsInZlcnNpb24iOiIzIiwibGFuZ3VhZ2UiOiJydSIsImRlc2NyaXB0aW9uIjoi0LzQvtC5INC60L7QvNC80LXQvdGC0LDRgNC40LkifQ=="
