module Main (main) where

import Test.HUnit
import Test.QuickCheck
import RLETest

main :: IO ()
main = do
    putStrLn "Running all tests:"
    _ <- runTestTT $ TestList
        [
            RLETest.testCompress,
            RLETest.testUncompress
        ]
    quickCheck propertyCompressUncompress
    putStrLn "All test runned."
