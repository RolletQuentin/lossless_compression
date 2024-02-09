module Main (main) where

import RLE

main :: IO ()
main = do
    let text = "aaabbccccdeffghhhijjj"
    let compressed = RLE.compress text
    print compressed
    let uncompressed = RLE.uncompress compressed
    print uncompressed
    