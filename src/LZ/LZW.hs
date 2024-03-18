{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : SOULIER Patrice
-}

module LZ.LZW(compress, uncompress) where

import LZ.Dictionaries

-- !!!!!!!!!!!!!!! VOIR SI AUTORISEE SINON RECODER A LA MAIN !!!!!!!!!!!!!!!
import Data.List (elemIndex)
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


-- | LZW compress method
compress :: String -> [Int]
compress [] = []
compress message = compress_method message 2 [] []

compress_method :: String -> Int -> [Int] -> [String] -> [Int]
-- Correspond à :  compress_method(message, n, code, dictionnaire) = code
  
-- Condition d'arrêt message vide
compress_method [] _ list_int _ = list_int
  
-- Condition d'arrêt length(message) = n    <=>  mon dernier patern est un patern que je connais.
compress_method message (length message) list_int dictionary = 
  list_int ++ [(elemIndex (take (length message) message) dictionary)]
  where dictionary = dictionary ++ [message]

-- Tous les autres cas
compress_method message n list_int dictionary =
  -- Soit message dans liste et n+1 < length(message)
  -- Si le patern crée avec les n premiers caractères de message est dans le dictionnaire on prend un caractère de plus
  | (take n message) `elem` dictionary = compress_method message (n+1) list_int dictionary
  -- Si le patern n'est pas dans la liste
  | otherwise = compress_method (drop (n-1) message) 2 (list_int ++ [(elemIndex (take n message) dictionary)]) dictionary
  -- !!!!!!!!!!!!!!!!! VERIFIER QUE CE WHERE S APPLIQUE QU A CE | !!!!!!!!!!!!!!!
  where dictionary = dictionary ++ [(take n message)]


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress _ = undefined -- TODO
