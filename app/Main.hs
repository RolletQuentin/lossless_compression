module Main (main) where

import RLE
import Statistic.EncodingTree
import Statistic.Huffman
import Statistic.ShannonFano

main :: IO ()
main = do
    let text = "je suis michel le fermier"

    let rleCompressed = RLE.compress text
    print rleCompressed
    let rleUncompressed = RLE.uncompress rleCompressed
    print rleUncompressed

    let (tree, huffmanCompressed) = Statistic.EncodingTree.compress Statistic.Huffman.tree text
    print huffmanCompressed
    let huffmanUncompressed = Statistic.EncodingTree.uncompress (tree, huffmanCompressed)
    print huffmanUncompressed
    
    let (shanontree, shanonCompressed) = Statistic.EncodingTree.compress Statistic.ShannonFano.tree text
    print shanonCompressed
    let shanonUncompressed = Statistic.EncodingTree.uncompress (shanontree, shanonCompressed)
    print shanonUncompressed