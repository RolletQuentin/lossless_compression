{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman (tree) where

import Statistic.EncodingTree
import Statistic.Source

import Data.List (sortBy)
import Data.Ord (comparing)

-- Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree string =
  case (orderedCounts string) of
    []             -> Nothing
    [(ltr, nbr)]   -> Just (EncodingLeaf nbr ltr)
    otherwise      -> Just (buildTree (sortBy (comparing snd) (copyToLeaf (orderedCounts string) [])))
  where
    copyToLeaf :: [(a, Int)] -> [((EncodingTree a), Int)] -> [((EncodingTree a), Int)]
    copyToLeaf [] newList = newList
    copyToLeaf ((ltr, nbr):rest) newList = copyToLeaf rest (((EncodingLeaf nbr ltr), nbr):newList)

    buildTree :: [((EncodingTree a), Int)] -> EncodingTree a
    buildTree [(tree, _)] = tree
    buildTree ((tree1, nbr1):(tree2, nbr2):rest) = buildTree (sortBy (comparing snd) (((EncodingNode (nbr1 + nbr2) tree1 tree2), (nbr1 + nbr2)) : rest))

    








