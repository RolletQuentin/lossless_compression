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
compress message = compress_method message 1 [] ascii

compress_method :: String -> Int -> [Int] -> [String] -> [Int]
-- Correspond à :  compress_method(message, n, code, dictionnaire) = code
  
-- Condition d'arrêt message vide
compress_method [] _ list_int _ = list_int
  

-- Tous les autres cas
compress_method message n list_int dictionary
  -- Condition d'arrêt, length(message) = n  <=>  mon dernier patern est un patern que je connais.
  | n == length message && (take n message) `elem` dictionary = final_list_int
  -- Soit message dans liste et n+1 <= length(message)
  -- Si le patern créé avec les n premiers caractères de message est dans le dictionnaire, on prend un caractère de plus
  | (take n message) `elem` dictionary = compress_method message (n+1) list_int dictionary
  -- Si le patern n'est pas dans la liste
  | otherwise = compress_method new_message 1 new_list_int updated_dictionary

  where 
    updated_dictionary = dictionary ++ [(take n message)]
    new_message = drop (n-1) message
    patern = take (n-1) message
    maybeIndex = elemIndex patern updated_dictionary
    -- On converti le maybeIndex en Index car on veut un Int et non un Maybe Int
    index_patern = case maybeIndex of
      Just index -> index
      Nothing -> -1
    new_list_int = list_int ++ [index_patern]
    -- Pour le final list int on ajoute le pattern entier (c'est le cas où j'ai un pattern à la fin de mon message que je connais dans le dico)
    maybeIndexFinalPatern = elemIndex message updated_dictionary
    index_final_patern = case maybeIndexFinalPatern of
      Just index -> index
      Nothing -> -1
    final_list_int = list_int ++ [index_final_patern]


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String

-- Condition d'arrêt message vide
uncompress [] = Nothing
uncompress code = uncompress_method new_code decode w ascii
  where
      v = head code -- La première valeur du code
      w = ascii !! v -- La lettre qui correspond à l'indice de v dans le dictionnaire
      decode = w
      new_code = tail code -- On ne garde que les nombres non déchiffré dans la liste code

uncompress_method :: [Int] -> String -> String -> [String] -> Maybe String
-- uncompress_method (code, decode, w, dictionary) = new_decode
uncompress_method code decode w dictionary
  -- Cas d'arrêt : code est vide
  | length code < 1 = Just decode
  -- Si la lettre correspondant à l'indice du code est dans le dictionary, on l'add à decode et on ajoute au dictionary (w + new_w[0])
  | new_w `elem` dictionary = uncompress_method new_code (decode ++ new_w) new_w (dictionary ++ [(w ++ [head new_w])]) 
  -- Sinon, on a new_w = w + w[0] et on ajoute également new_w au dictionnaire
  | otherwise = uncompress_method new_code (decode ++ (w ++ [head w])) (w ++ [head w]) (dictionary ++ [(w ++ [head w])]) 
  where 
    v = head code -- Prochain nombre du code à déchiffrer
    new_code = tail code -- On ne garde que les nombres non déchiffré dans la liste code
    new_w = ascii !! v -- Valeur dans le dictionnaire correspondant à l'indice du code à déchiffrer




