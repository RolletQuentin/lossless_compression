-- ShannonFanoTest.hs
module StatisticTest.ShannonFanoTest (
    testShannonFano
    ) where

import Test.HUnit
import Statistic.EncodingTree
import Statistic.ShannonFano

-- Helper function to build an EncodingTree for testing
buildEncodingTree :: String -> Maybe (EncodingTree Char)
buildEncodingTree xs = tree xs

-- Test cases for ShannonFano tree generation
testShannonFano :: Test
testShannonFano = TestList
    [
        "Empty input" ~: buildEncodingTree "" ~?= Nothing,
        "Single character input" ~: buildEncodingTree "a" ~?= Just (EncodingLeaf 1 'a'),
        "Multiple characters with different frequencies" ~:
            buildEncodingTree "abcdd" ~?= Just (EncodingNode 5
                (EncodingLeaf 1 'a')
                (EncodingNode 4
                    (EncodingLeaf 1 'b')
                    (EncodingNode 3
                        (EncodingNode 2
                            (EncodingLeaf 1 'd')
                            (EncodingLeaf 1 'c')
                        )
                        (EncodingLeaf 1 'd')
                    )
                )
            ),
        "Multiple characters with equal frequencies" ~:
            buildEncodingTree "abcd" ~?= Just (EncodingNode 4
                (EncodingNode 2
                    (EncodingLeaf 1 'b')
                    (EncodingLeaf 1 'a')
                )
                (EncodingNode 2
                    (EncodingLeaf 1 'd')
                    (EncodingLeaf 1 'c')
                )
            )
    ]

