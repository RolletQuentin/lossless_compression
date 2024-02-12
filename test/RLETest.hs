-- RLETest.hs
module RLETest where

import Test.HUnit
import RLE

-- Test cases for compress function
testCompress :: Test
testCompress = TestList
    [ "Empty input" ~: compress ([] :: [Int]) ~?= ([] :: [(Int, Int)])
    , "Single element" ~: compress [1] ~?= [(1, 1)]
    , "Consecutive elements" ~: compress [1, 1, 2, 2, 2] ~?= [(1, 2), (2, 3)]
    , "Non-consecutive elements" ~: compress [1, 2, 1, 2, 1] ~?= [(1, 1), (2, 1), (1, 1), (2, 1), (1, 1)]
    ]

main :: IO ()
main = defaultMain [testCompress]
