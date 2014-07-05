module Liqpay.Tests.CoderSpec where

import Test.Hspec
--import Test.QuickCheck
--import Control.Exception (evaluate)

import Liqpay.Coder
import Data.ByteString.Lazy.Char8 as BLC

tests :: IO ()
tests = hspec $ do
    describe "Coder.encodeSignature" $ do
        it "returns encoded ByteString" $ do
            encodeSignature "test" `shouldBe` (BLC.pack "qUqP5cyxm6YcTAhz05Hph5gvu9M=")
