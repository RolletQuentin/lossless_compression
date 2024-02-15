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
    , "ASCII input" ~: compress "ABABAB" ~?= [65, 66, 256, 258]
    , "Repeated pattern" ~: compress "ABCABCABC" ~?= [65, 66, 67, 256, 258]
    ]

-- Test cases for LZW uncompress function
testLZWUncompress :: Test
testLZWUncompress = TestList
    [ "Empty input" ~: uncompress [] ~?= Just ""
    , "ASCII input" ~: uncompress [65, 66, 256, 258] ~?= Just "ABABAB"
    , "Repeated pattern" ~: uncompress [65, 66, 67, 256, 258] ~?= Just "ABCABCABC"
    ]

-- QuickCheck property for LZW compress/uncompress: uncompressed string
-- should be the same than before
propertyLZWCompressUncompress :: String -> Property
propertyLZWCompressUncompress xs =
    let compressed = compress xs
        uncompressed = uncompress compressed
    in uncompressed === Just xs
