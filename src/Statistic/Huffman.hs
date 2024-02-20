{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman (tree) where

import Statistic.EncodingTree

-- Count occurrences of each symbol
countOccurrences :: Eq a => [a] -> [(a, Int)]
countOccurrences [] = []
countOccurrences (x:xs) = (x, countElement x (x:xs)) : countOccurrences (filter (/=x) xs)
  where
    countElement _ [] = 0
    countElement y (z:zs)
      | y == z = 1 + countElement y zs
      | otherwise = countElement y zs

-- Simple bubble sort to sort the list of tuples based on frequency
bubbleSort :: Ord b => [(a, b)] -> [(a, b)]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort ((x1, y1):(x2, y2):xs)
  | y1 <= y2 = (x1, y1) : bubbleSort ((x2, y2):xs)
  | otherwise = (x2, y2) : bubbleSort ((x1, y1):xs)

-- Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree xs = Just $ buildTree $ map (\(s, f) -> EncodingLeaf f s) $ bubbleSort $ countOccurrences xs
  where
    buildTree [node] = node
    buildTree (node1:node2:nodes) = buildTree $ insertByWeight (mergeNodes node1 node2) nodes
    mergeNodes n1 n2 = EncodingNode (count n1 + count n2) n1 n2
    insertByWeight node [] = [node]
    insertByWeight node (x:xs)
      | count node <= count x = node : x : xs
      | otherwise = x : insertByWeight node xs






