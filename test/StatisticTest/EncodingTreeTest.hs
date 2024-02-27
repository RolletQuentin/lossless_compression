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
        "Is Leaf - True" ~: isLeaf (EncodingLeaf 1 'a') ~?= True,
        "Is Leaf - False" ~: isLeaf (EncodingNode 2 (EncodingLeaf 1 'a') (EncodingLeaf 1 'b')) ~?= False,
        "Count - Leaf" ~: count (EncodingLeaf 5 'a') ~?= 5,
        "Count - Node" ~: count (EncodingNode 10 (EncodingLeaf 3 'a') (EncodingLeaf 7 'b')) ~?= 10,
        "Has - True" ~: has (EncodingLeaf 1 'a') 'a' ~?= True,
        "Has - False" ~: has (EncodingLeaf 1 'a') 'b' ~?= False,
        "Encode" ~: encode (EncodingLeaf 1 'a') 'a' ~?= Just [],
        "DecodeOnce" ~: decodeOnce (EncodingNode 5 (EncodingLeaf 3 'a') (EncodingLeaf 2 'b')) [Zero, One, Zero, Zero, One] ~?= Just ('a', [One, Zero, Zero, One]),
        "Decode - Empty" ~: decode (EncodingLeaf 1 'a') [] ~?= Just "a",
        "Decode - Valid" ~: decode (EncodingNode 2 (EncodingLeaf 1 'a') (EncodingLeaf 1 'b')) [Zero, One] ~?= Just "ab",
        "MeanLength - Leaf" ~: meanLength (EncodingLeaf 1 'a') ~?= 0.0,
        "MeanLength - Node" ~: meanLength (EncodingNode 10 (EncodingLeaf 3 'a') (EncodingLeaf 7 'b')) ~?= 1.0
    ]


-- Votre propriété pour Huffman
propertyHuffmanCompressUncompress :: Property
propertyHuffmanCompressUncompress =
  forAll (arbitrary `suchThat` (\xs -> length xs >= 2)) $ \xs ->
    let (encodingTree, compressed) = compress Statistic.Huffman.tree xs
        uncompressed = uncompress (encodingTree, compressed)
    in counterexample ("Input: " ++ xs) $ encodingTree === Statistic.Huffman.tree xs .&&. uncompressed === Just xs

-- Votre propriété pour Shannon-Fano
propertyShannonFanoCompressUncompress :: Property
propertyShannonFanoCompressUncompress =
  forAll (arbitrary `suchThat` (\xs -> length xs >= 2)) $ \xs ->
    let (encodingTree, compressed) = compress Statistic.ShannonFano.tree xs
        uncompressed = uncompress (encodingTree, compressed)
    in counterexample ("Input: " ++ xs) $ encodingTree === Statistic.ShannonFano.tree xs .&&. uncompressed === Just xs





