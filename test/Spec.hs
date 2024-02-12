module Main where

import Test.HUnit
import Test.QuickCheck
import RLETest

main :: IO ()
main = do
    putStrLn "Running all tests:"
    runTestTT $ TestList
        [
            RLETest.testCompress
        ]
