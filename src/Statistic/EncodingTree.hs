{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : Noailles Valentin
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ ltr) target = ltr == target
has (EncodingNode _ lft rgt) ltr = has lft ltr || has rgt ltr

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ ltr) target
  | ltr == target = Just []
  | otherwise     = Nothing
encode (EncodingNode _ lft rgt) target
  | has lft target = fmap (Zero:) (encode lft target)
  | has rgt target = fmap (One:) (encode rgt target)
  | otherwise      = Nothing

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ ltr) rest = Just (ltr, rest)
decodeOnce (EncodingNode _ lft rgt) (b:rest)
  | b == Zero = decodeOnce lft rest
  | b == One  = decodeOnce rgt rest
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode (EncodingLeaf nbr ltr) _ = 
  Just (decodeOnlyLeaf nbr ltr)
    where
      decodeOnlyLeaf :: Int -> a -> [a]
      decodeOnlyLeaf 0 _ = []
      decodeOnlyLeaf nbr ltr = ltr:(decodeOnlyLeaf (nbr - 1) ltr)
decode _ [] = Just []
decode tree bits = do
  (ltr, rest) <- decodeOnce tree bits
  decodedRest <- decode tree rest
  Just (ltr : decodedRest)

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = 
  fromIntegral (meanLengthHelper tree 0) / fromIntegral (count tree)
    where
      meanLengthHelper :: EncodingTree a -> Int -> Int
      meanLengthHelper (EncodingLeaf nbr _) depth = nbr * depth
      meanLengthHelper (EncodingNode _ lft rgt) depth =
        let left = meanLengthHelper lft (depth + 1)
            right = meanLengthHelper rgt (depth + 1)
        in left + right -- check if i can remove in by putting 2 let instead of 1

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress treeGenerator text = 
  case treeGenerator text of
    Just tree -> case encodeText tree text of 
                  Just bits -> (Just tree, bits)
                  Nothing   -> (Nothing, []) 
    Nothing   -> (Nothing, [])
    where
      encodeText :: Eq a => EncodingTree a -> [a] -> Maybe [Bit]
      encodeText _ [] = Just []
      encodeText (EncodingLeaf _ _) _ = Just []
      encodeText tree (ltr:rest) = case encode tree ltr of
        Just bits -> case encodeText tree rest of 
                      Just bit -> Just (bits ++ bit)
                      Nothing  -> Nothing
        Nothing   -> Just []

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Just tree, bits) = decode tree bits
uncompress _ = Nothing

