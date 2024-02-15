-- HuffmanTest.hs
module StatisticTest.HuffmanTest (
    testHuffmanTree
    ) where

import Test.HUnit
import Statistic.EncodingTree
import Statistic.Huffman

-- Test cases for Huffman tree generation
testHuffmanTree :: Test
testHuffmanTree = TestList
    [ 
        "Huffman Tree - Empty Input" ~: tree ([] :: [Char]) ~?= Nothing,
        "Huffman Tree - Single Symbol" ~: tree "a" ~?= Just (EncodingLeaf 1 'a'),
        "Huffman Tree - Multiple Symbols" ~: tree "aabcc" ~?= Just (EncodingNode 5 (EncodingNode 2 (EncodingLeaf 1 'b') (EncodingLeaf 1 'c')) (EncodingLeaf 2 'a'))
    ]
