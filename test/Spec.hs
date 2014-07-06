module Main where

import CoderSpec as C (tests)
import LiqpaySpec as L (tests)

main :: IO ()
main = do
    C.tests
    L.tests
