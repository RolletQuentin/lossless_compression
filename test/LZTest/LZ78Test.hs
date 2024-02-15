-- LZ78Test.hs
module LZTest.LZ78Test (
    testLZ78Compress,
    testLZ78Uncompress,
    propertyLZ78CompressUncompress
    ) where

import Test.HUnit
import Test.QuickCheck
import LZ.LZ78

-- Test cases for LZ78 compress function
testLZ78Compress :: Test
testLZ78Compress = TestList
    [
        "Empty input" ~: compress "" ~?= [],
        "ASCII input" ~: compress "ABABAB" ~?= [(0, 'A'), (0, 'B'), (2, 'A'), (4, 'B')],
        "Reapeated pattern" ~: compress "ABCABCABC" ~?= [(0, 'A'), (0, 'B'), (0, 'C'), (3, 'A'), (5, 'B')]
    ]

-- Test cases for LZ78 uncompress function
testLZ78Uncompress :: Test
testLZ78Uncompress = TestList
    [ 
        "Empty input" ~: uncompress [] ~?= Just "",
        "ASCII input" ~: uncompress [(0, 'A'), (0, 'B'), (2, 'A'), (4, 'B')] ~?= Just "ABABAB",
        "Repeated pattern" ~: uncompress [(0, 'A'), (0, 'B'), (0, 'C'), (3, 'A'), (5, 'B')] ~?= Just "ABCABCABC"
    ]

-- QuickCheck property for LZ78 compress/uncompress: uncompressed string
-- should be the same than before
propertyLZ78CompressUncompress :: String -> Property
propertyLZ78CompressUncompress xs =
    let compressed = compress xs
        uncompressed = uncompress compressed
    in uncompressed === Just xs