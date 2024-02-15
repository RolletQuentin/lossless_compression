-- EncodingTreeTest.hs
module StatisticTest.EncodingTreeTest (
    testEncodingTree,
    propertyHuffmanCompressUncompress,
    propertyShannonFanoCompressUncompress
    ) where

import Test.HUnit
import Test.QuickCheck
import Statistic.Bit
import Statistic.EncodingTree
import Statistic.Huffman
import Statistic.ShannonFano

-- Test cases for EncodingTree functions
testEncodingTree :: Test
testEncodingTree = TestList
    [ 
        "Is Leaf - True" ~: isLeaf (EncodingLeaf 0 'a') ~?= True,
        "Is Leaf - False" ~: isLeaf (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) ~?= False,
        "Count - Leaf" ~: count (EncodingLeaf 5 'a') ~?= 5,
        "Count - Node" ~: count (EncodingNode 10 (EncodingLeaf 3 'a') (EncodingLeaf 7 'b')) ~?= 10,
        "Has - True" ~: has (EncodingLeaf 0 'a') 'a' ~?= True,
        "Has - False" ~: has (EncodingLeaf 0 'a') 'b' ~?= False,
        "Encode" ~: encode (EncodingLeaf 0 'a') 'a' ~?= Just [Zero],
        "DecodeOnce" ~: decodeOnce (EncodingLeaf 0 'a') [Zero] ~?= Just ('a', []),
        "DecodeOnce - Incomplete" ~: decodeOnce (EncodingLeaf 0 'a') [] ~?= Nothing,
        "DecodeOnce - Invalid Symbol" ~: decodeOnce (EncodingLeaf 0 'a') [One] ~?= Nothing,
        "Decode - Empty" ~: decode (EncodingLeaf 0 'a') [] ~?= Just "",
        "Decode - Valid" ~: decode (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) [Zero, One] ~?= Just "ab",
        "MeanLength - Leaf" ~: meanLength (EncodingLeaf 0 'a') ~?= 0.0,
        "MeanLength - Node" ~: meanLength (EncodingNode 10 (EncodingLeaf 3 'a') (EncodingLeaf 7 'b')) ~?= 1.0
    ]


-- QuickCheck property for Huffman compress/uncompress: uncompressed list
-- should be the same than before
propertyHuffmanCompressUncompress :: String -> Property
propertyHuffmanCompressUncompress xs =
    let (encodingTree, compressed) = compress Statistic.Huffman.tree xs
        uncompressed = uncompress (encodingTree, compressed)
    in counterexample ("Input: " ++ xs) $ encodingTree === Statistic.Huffman.tree xs .&&. uncompressed === Just xs

-- QuickCheck property for Shannon-Fano compress/uncompress: uncompressed list
-- should be the same than before
propertyShannonFanoCompressUncompress :: String -> Property
propertyShannonFanoCompressUncompress xs =
    let (encodingTree, compressed) = compress Statistic.ShannonFano.tree xs
        uncompressed = uncompress (encodingTree, compressed)
    in counterexample ("Input: " ++ xs) $ encodingTree === Statistic.ShannonFano.tree xs .&&. uncompressed === Just xs




