{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : Marc-Antoine VERGNET
-}
module LZ.LZ78(compress, uncompress) where

isOnDictionaries :: Char -> Int -> [(Int, Char)] -> (Bool, Int)
isOnDictionaries _ _ [] = (False, -1)
isOnDictionaries char index dict = 
  isOnDictionariesHelper char index dict 1
    where
      isOnDictionariesHelper :: Char -> Int -> [(Int, Char)] -> Int -> (Bool, Int)
      isOnDictionariesHelper _ _ [] _ = (False, -1)
      isOnDictionariesHelper char index ((dict_index, dict_char):dict) position
          | char == dict_char && index == dict_index = (True, position)
          | otherwise = isOnDictionariesHelper char index dict (position + 1)

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress [] = []
compress string = 
  compressHelper string 0 []
    where
      compressHelper :: String -> Int -> [(Int, Char)] -> [(Int, Char)] 
      compressHelper [] _ dict = dict
      compressHelper string index dict = 
        let firstChar = take 1 string
            tail = drop 1 string
            (found, dictIndex) = isOnDictionaries (head firstChar) index dict       
        in if found then
             compressHelper tail dictIndex dict
           else 
             compressHelper tail 0 (dict ++ [(index, head firstChar)])




-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress _ = undefined -- TODO




