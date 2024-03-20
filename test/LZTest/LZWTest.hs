-- LZWTest.hs
module LZTest.LZWTest (
    testLZWCompress,
    testLZWUncompress,
    propertyLZWCompressUncompress
    ) where

import Test.HUnit
import Test.QuickCheck
import LZ.LZW

-- Test cases for LZW compress function
testLZWCompress :: Test
testLZWCompress = TestList
    [ "Empty input" ~: compress "" ~?= []
    , "ASCII input" ~: compress "ABABAB" ~?= [65, 66, 256, 256]
    , "Repeated pattern" ~: compress "ABCABCABC" ~?= [65, 66, 67, 256, 258, 257]
    ]

-- Test cases for LZW uncompress function
testLZWUncompress :: Test
testLZWUncompress = TestList
    [ "Empty input" ~: uncompress [] ~?= Nothing
    , "ASCII input" ~: uncompress [65, 66, 256, 256] ~?= Just "ABABAB"
    , "Repeated pattern" ~: uncompress [65, 66, 67, 256, 258, 257] ~?= Just "ABCABCABC"
    ]

asciiChars :: String
asciiChars = ['\NUL' .. '\255']

-- QuickCheck property for LZW compress/uncompress: uncompressed string should be the same as before
propertyLZWCompressUncompress :: Property
propertyLZWCompressUncompress =
    forAll (listOf1 $ elements asciiChars) $ \xs ->
        let compressed = compress xs
            uncompressed = uncompress compressed
        in counterexample ("Input: " ++ xs) $ uncompressed === Just xs
