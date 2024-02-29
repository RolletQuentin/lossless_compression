{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map.Map a Int
occurrences = foldr updateMap Map.empty
  where
    updateMap :: Ord a => a -> Map.Map a Int -> Map.Map a Int
    updateMap key map = Map.insertWith (+) key 1 map

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts string = sortBy (comparing snd) (Map.toList (occurrences string))

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy string = 
  let total = fromIntegral (sum (map snd (orderedCounts string)))
      entropyTerm (_, nbr) = 
        let p = fromIntegral nbr / total 
        in -p * logBase 2 p
  in sum (map entropyTerm (orderedCounts string))
