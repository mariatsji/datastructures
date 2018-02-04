module Huffman (Huffman(..), Encoded(..), val, toMap, huffmanTree, encode) where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Ord

data Huffman = Leaf Int Char | Node Int Huffman Huffman deriving (Eq, Ord, Show)
newtype Encoded = Enc (Huffman, [Bool]) deriving (Eq, Ord, Show)

encode :: String -> Encoded
encode s = let tree = mkHuffmanTree s
           in Enc (tree, encode' tree s)

encode' :: Huffman -> String -> [Bool]
encode' h s = s >>= (\c -> encode'' h c [])

encode'' :: Huffman -> Char -> [Bool] -> [Bool]
encode'' (Leaf _ c1) c bs = if c == c1 then bs else []
encode'' (Node _ h1 h2) c bs = encode'' h1 c (bs ++ [False]) ++ encode'' h2 c (bs ++ [True])

mkHuffmanTree :: String -> Huffman
mkHuffmanTree = huffmanTree . sort' . leafify

huffmanTree :: [Huffman] -> Huffman
huffmanTree (x:[]) = x
huffmanTree (x:(y:(z))) = huffmanTree $ Node (val x + val y) x y : z

sort' :: [Huffman] -> [Huffman]
sort' hs = List.sortBy comp hs
  where comp = comparing val

leafify :: String -> [Huffman]
leafify s = fmap (\(c,i) -> Leaf i c) $ Map.toList $ toMap s

toMap :: String -> Map.Map Char Int
toMap s = foldl (\a c -> Map.insertWith (+) c 1 a) (Map.empty :: Map.Map Char Int) s

val :: Huffman -> Int
val (Leaf i _) = i
val (Node i _ _) = i
