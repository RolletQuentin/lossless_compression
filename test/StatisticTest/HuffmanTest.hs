-- HuffmanTest.hs
module StatisticTest.HuffmanTest (
    testHuffmanTree
    ) where

import Test.HUnit
import Statistic.EncodingTree
import Statistic.Huffman
import qualified Data.Tree as T

-- To transform a Huffman Tree in Generic Tree
toGenericTree :: EncodingTree a -> T.Tree (Int, Maybe a)
toGenericTree (EncodingLeaf cnt ltr) = T.Node (cnt, Just ltr) []
toGenericTree (EncodingNode cnt lft rgt) = T.Node (cnt, Nothing) [toGenericTree lft, toGenericTree rgt]

-- Test cases for Huffman tree generation
testHuffmanTree :: Test
testHuffmanTree = TestList
    [ 
        "Huffman Tree - Empty Input" ~: tree ([] :: [Char]) ~?= Nothing,
        "Huffman Tree - Single Symbol" ~: tree "a" ~?= Just (EncodingLeaf 1 'a'),
        "Huffman Tree - Multiple Symbols" ~: toGenericTree <$> tree "aabcc" ~?= Just (toGenericTree (EncodingNode 5 (EncodingLeaf 2 'c') (EncodingNode 3 (EncodingLeaf 1 'b') (EncodingLeaf 2 'a'))))
    ]
