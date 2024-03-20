-- SourceTest.hs
module StatisticTest.SourceTest (
    testOccurrences,
    testEntropy,
    testOrderedCounts
    ) where

import Test.HUnit
import qualified Data.Map as Map
import Statistic.Source

-- Test cases for occurrences function
testOccurrences :: Test
testOccurrences = TestList
    [
        "Empty input" ~: occurrences "" ~?= Map.empty,
        "Single character input" ~: occurrences "a" ~?= Map.fromList [('a', 1)],
        "Multiple characters input" ~: occurrences "abcabc" ~?= Map.fromList [('a', 2), ('b', 2), ('c', 2)]
    ]

-- Test cases for entropy function
testEntropy :: Test
testEntropy = TestList
    [
        "Empty input" ~: entropy "" ~?= 0.0,
        "Single character input" ~: entropy "a" ~?= 0.0,
        "Equal probabilities" ~: entropy "abcd" ~?= 2.0,
        "Unequal probabilities" ~: entropy "aabbc" ~?= 1.5219280948873621
    ]

-- Test cases for orderedCounts function
testOrderedCounts :: Test
testOrderedCounts = TestList
    [
        "Empty input" ~: orderedCounts "" ~?= [],
        "Single character input" ~: orderedCounts "a" ~?= [('a', 1)],
        "Multiple characters input" ~: orderedCounts "abcabc" ~?= [('a', 2), ('b', 2), ('c', 2)]
    ]
