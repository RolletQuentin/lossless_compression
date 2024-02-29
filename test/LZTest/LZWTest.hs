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
    , "ASCII input" ~: compress "ABABAB" ~?= [65, 66, 128, 128]
    , "Repeated pattern" ~: compress "ABCABCABC" ~?= [65, 66, 67, 128, 130, 129]
    ]

-- Test cases for LZW uncompress function
testLZWUncompress :: Test
testLZWUncompress = TestList
    [ "Empty input" ~: uncompress [] ~?= Nothing
    , "ASCII input" ~: uncompress [65, 66, 128, 128] ~?= Just "ABABAB"
    , "Repeated pattern" ~: uncompress [65, 66, 67, 128, 130, 129] ~?= Just "ABCABCABC"
    ]

-- QuickCheck property for LZW compress/uncompress: uncompressed string
-- should be the same than before
propertyLZWCompressUncompress :: Property
propertyLZWCompressUncompress =
    forAll (arbitrary `suchThat`(\xs -> length xs >= 1)) $ \xs ->
        let compressed = compress xs
            uncompressed = uncompress compressed
        in counterexample ("Input: " ++ xs) $ uncompressed === Just xs
