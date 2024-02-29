module Statistic.ShannonFano (tree) where

import Statistic.EncodingTree
import Statistic.Source

import Data.List (sortBy)
import Data.Ord (comparing)

-- Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree string = 
  Just (buildTree (copyToLeaf (orderedCounts string) []))
    where
      copyToLeaf :: [(a, Int)] -> [((EncodingTree a), Int)] -> [((EncodingTree a), Int)]
      copyToLeaf [] newList = newList
      copyToLeaf ((ltr, nbr):rest) newList = copyToLeaf rest (((EncodingLeaf nbr ltr), nbr):newList)

      buildTree :: Ord a => [(EncodingTree a, Int)] -> EncodingTree a
      buildTree [(tree, _)] = tree
      buildTree nodes =
        let (left, right) = splitAt (length nodes `div` 2) nodes
        in EncodingNode (sum (map snd nodes)) (buildTree left) (buildTree right)