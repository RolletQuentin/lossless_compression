module Main (main) where

import Test.HUnit
import Test.QuickCheck
import RLETest
import LZTest.LZ78Test
import LZTest.LZWTest
import StatisticTest.EncodingTreeTest
import StatisticTest.HuffmanTest
import StatisticTest.ShannonFanoTest
import StatisticTest.SourceTest

main :: IO ()
main = do
    putStrLn "Running all tests:\n"

    putStrLn "Running test for RLE..."
    _ <- runTestTT $ TestList
        [
            RLETest.testCompress,
            RLETest.testUncompress
        ]
    quickCheck propertyCompressUncompress
    putStrLn "RLE tests are finnished.\n"

    -- putStrLn "Running test for LZ78..."
    -- _ <- runTestTT $ TestList
    --     [
    --         LZTest.LZ78Test.testLZ78Compress,
    --         LZTest.LZ78Test.testLZ78Uncompress
    --     ]
    -- quickCheck propertyLZ78CompressUncompress
    -- putStrLn "LZ78 tests are finnished.\n"

    -- putStrLn "Running test for LZW..."
    -- _ <- runTestTT $ TestList
    --     [
    --         LZTest.LZWTest.testLZWCompress,
    --         LZTest.LZWTest.testLZWUncompress
    --     ]
    -- quickCheck propertyLZWCompressUncompress
    -- putStrLn "LZW tests are finnished.\n"

    putStrLn "Running test for Encoding Tree..."
    _ <- runTestTT $ TestList
        [
            StatisticTest.EncodingTreeTest.testEncodingTree
        ]
    quickCheck propertyHuffmanCompressUncompress
    -- quickCheck propertyShannonFanoCompressUncompress
    putStrLn "Encoding Tree tests are finnished.\n"

    putStrLn "Running test for Huffman..."
    _ <- runTestTT $ TestList
        [
            StatisticTest.HuffmanTest.testHuffmanTree
        ]
    putStrLn "Huffman tests are finnished.\n"

    -- putStrLn "Running test for Shannon-Fano..."
    -- _ <- runTestTT $ TestList
    --     [
    --         StatisticTest.ShannonFanoTest.testShannonFano
    --     ]
    -- putStrLn "Shannon-Fano tests are finnished.\n"

    -- putStrLn "Running test for Source..."
    -- _ <- runTestTT $ TestList
    --     [
    --         StatisticTest.SourceTest.testOccurrences,
    --         StatisticTest.SourceTest.testEntropy,
    --         StatisticTest.SourceTest.testOrderedCounts
    --     ]
    -- putStrLn "Source tests are finnished.\n"

    putStrLn "All test runned."
