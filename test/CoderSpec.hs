module CoderSpec where

import Test.Hspec
import Data.ByteString.Lazy.Char8 as BLC
import Liqpay.Coder

tests :: IO ()
tests = hspec $ do
    describe "Coder.encodeSignature" $ do
        it "returns encoded ByteString" $ do
            encodeSignature "test" `shouldBe` BLC.pack "qUqP5cyxm6YcTAhz05Hph5gvu9M="
