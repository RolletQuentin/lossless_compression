module Main (main) where

import RLE
import Statistic.EncodingTree
import Statistic.Huffman

main :: IO ()
main = do
    let text = "aaabbccccdeffghhhijjj"

    let rleCompressed = RLE.compress text
    print rleCompressed
    let rleUncompressed = RLE.uncompress rleCompressed
    print rleUncompressed

    let (tree, huffmanCompressed) = Statistic.EncodingTree.compress Statistic.Huffman.tree text
    print huffmanCompressed
    let huffmanUncompressed = Statistic.EncodingTree.uncompress (tree, huffmanCompressed)
    print huffmanUncompressed
    