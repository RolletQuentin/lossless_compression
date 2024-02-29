-- LZ78Test.hs
module LZTest.LZ78Test (
    testLZ78Compress,
    testLZ78Uncompress,
    propertyLZ78CompressUncompress
    ) where

import Test.HUnit
import Test.QuickCheck
import LZ.LZ78
import Data.List (elem)

-- Test cases for LZ78 compress function
testLZ78Compress :: Test
testLZ78Compress = TestList
    [
        "Empty input" ~: compress "" ~?= [],
        "ASCII input" ~: compress "ABABAB" ~?= [(0, 'A'), (0, 'B'), (1, 'B'), (3, '\NUL')],
        "Reapeated pattern" ~: compress "ABCABCABC" ~?= [(0, 'A'), (0, 'B'), (0, 'C'), (1, 'B'), (3, 'A'), (2, 'C')]
    ]

-- Test cases for LZ78 uncompress function
testLZ78Uncompress :: Test
testLZ78Uncompress = TestList
    [ 
        "Empty input" ~: uncompress [] ~?= Nothing,
        "ASCII input" ~: uncompress [(0, 'A'), (0, 'B'), (1, 'B'), (3, '\NUL')] ~?= Just "ABABAB",
        "Repeated pattern" ~: uncompress [(0, 'A'), (0, 'B'), (0, 'C'), (1, 'B'), (3, 'A'), (2, 'C')] ~?= Just "ABCABCABC"
    ]

-- QuickCheck property for LZ78 compress/uncompress: uncompressed string
-- should be the same than before
propertyLZ78CompressUncompress :: Property
propertyLZ78CompressUncompress =
    forAll (arbitrary `suchThat`(\xs -> length xs >= 1 && not (elem '\NUL' xs))) $ \xs ->
        let compressed = compress xs
            uncompressed = uncompress compressed
        in counterexample ("Input: " ++ xs) $ uncompressed === Just xs