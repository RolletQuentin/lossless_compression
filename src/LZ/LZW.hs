{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : SOULIER Patrice
-}

module LZ.LZW(compress, uncompress) where

import LZ.Dictionaries

-- !!!!!!!!!!!!!!! VOIR SI AUTORISEE SINON RECODER A LA MAIN !!!!!!!!!!!!!!!
-- Val a dit autorisé
import Data.List (elemIndex)
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



-- | LZW compress method
compress :: String -> [Int]
compress [] = []
compress message = compress_method message 2 [] ascii

compress_method :: String -> Int -> [Int] -> [String] -> [Int]
-- Correspond à :  compress_method(message, n, code, dictionnaire) = code
  
-- Condition d'arrêt message vide
compress_method [] _ list_int _ = list_int
  
-- Condition d'arrêt length(message) = n    <=>  mon dernier patern est un patern que je connais.
compress_method message length_message list_int dictionary = 
  list_int ++ index_patern
  where 
    updated_dictionary = dictionary ++ [message]
    length_message = length message
    patern = take length_message message
    maybeIndex = elemIndex patern updated_dictionary
    index_patern = case maybeIndex of
      Just index -> [index]
      Nothing -> []

-- Tous les autres cas
compress_method message n list_int dictionary
  -- Soit message dans liste et n+1 < length(message)
  -- Si le patern crée avec les n premiers caractères de message est dans le dictionnaire on prend un caractère de plus
  | (take n message) `elem` dictionary = compress_method message (n+1) list_int dictionary
  -- Si le patern n'est pas dans la liste
  | otherwise = compress_method new_message 2 new_list_int updated_dictionary
  -- Le where s'applique qu'à mon otherwise
  where 
    updated_dictionary = dictionary ++ [(take n message)]
    new_message = drop (n-1) message
    patern = take n message
    maybeIndex = elemIndex patern updated_dictionary
    -- On converti le maybeIndex en Index car on veut un Int et non un Maybe Int
    index_patern = case maybeIndex of
      Just index -> index
      Nothing -> -1
    new_list_int = list_int ++ [index_patern]


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress _ = undefined -- TODO
