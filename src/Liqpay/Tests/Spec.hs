module Main where

import Liqpay.Tests.CoderSpec as C (tests)
import Liqpay.Tests.LiqpaySpec as L (tests)

main :: IO ()
main = do
    C.tests
    L.tests
