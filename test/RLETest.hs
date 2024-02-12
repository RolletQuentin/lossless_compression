-- RLETest.hs
module RLETest (
    testCompress,
    testUncompress,
    propertyCompressUncompress
    ) where

import Test.HUnit
import Test.QuickCheck
import RLE

-- Test cases for compress function
testCompress :: Test
testCompress = TestList
    [ "Empty input" ~: compress ([] :: [Int]) ~?= ([] :: [(Int, Int)])
    , "Single element" ~: compress [1 :: Int] ~?= [(1, 1)]
    , "Consecutive elements" ~: compress [1, 1, 2, 2, 2 :: Int] ~?= [(1, 2), (2, 3)]
    , "Non-consecutive elements" ~: compress [1, 2, 1, 2, 1 :: Int] ~?= [(1, 1), (2, 1), (1, 1), (2, 1), (1, 1)]
    ]

-- Test cases for uncompress function
testUncompress :: Test
testUncompress = TestList
    [ "Empty input" ~: uncompress ([] :: [(Int, Int)]) ~?= Just ([] :: [Int])
    , "Single element" ~: uncompress [(1, 1) :: (Int, Int)] ~?= Just ([1 :: Int])
    , "Consecutive elements" ~: uncompress [(1, 2), (3, 4), (2, 2) :: (Int, Int)] ~?= Just ([1, 1, 3, 3, 3, 3, 2, 2 :: Int])
    , "Non-consecutive elements" ~: uncompress [(1, 2), (3, 1), (1, 2), (3, 2) :: (Int, Int)] ~?= Just ([1, 1, 3, 1, 1, 3, 3 :: Int])
    ]

-- Test property on compress/uncompress : uncompressed list
-- should be the same than before
propertyCompressUncompress :: [Int] -> Property
propertyCompressUncompress xs =
    let compressed = compress xs
        uncompressed = uncompress compressed
    in uncompressed === Just xs
