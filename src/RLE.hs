{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : Noailles Valentin
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress s = 
  compressHelper [] s
    where
      compressHelper :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
      compressHelper acc [] = reverse acc
      compressHelper [] (x:xs) = compressHelper [(x, 1)] xs
      compressHelper ((y, count):rest) (x:xs)
        | x == y    = compressHelper ((y, count + 1):rest) xs
        | otherwise = compressHelper ((x, 1):(y, count):rest) xs

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress array = 
  uncompressHelper [] array
    where
      uncompressHelper :: [a] -> [(a, Int)] -> Maybe [a]
      uncompressHelper acc [] = Just (reverse acc)
      uncompressHelper acc ((c, 1):rest) = uncompressHelper (c:acc) rest
      uncompressHelper acc ((c, n):rest) 
        | n > 1     = uncompressHelper (c:acc) ((c, n-1):rest)
        | otherwise = Nothing