module Huffman (Huffman(..), Encoded(..), val, toMap, huffmanTree, encode, decode) where

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           Data.Ord

data Huffman = Leaf Int Char | Node Int Huffman Huffman deriving (Eq, Ord, Show)
newtype Encoded = Enc (Huffman, [Bool]) deriving (Eq, Ord, Show)

encode :: String -> Encoded
encode s = let tree = mkHuffmanTree s
           in Enc (tree, encode' tree s)

decode :: Encoded -> String
decode (Enc (h, bs)) = decode' h h bs []

-- original complete tree -> partial huffman under examination -> bools to decode -> accumulated chars -> decoded String
decode' :: Huffman -> Huffman -> [Bool] -> String -> String
decode' original (Leaf _ c1) bs cs = decode' original original bs (cs ++ [c1])
decode' original h [] cs = cs
decode' original (Node _ h1 h2) (b:bs) cs
  | b = decode' original h2 bs cs
  | otherwise = decode' original h1 bs cs

encode' :: Huffman -> String -> [Bool]
encode' h s = s >>= (\c -> encode'' h c [])

encode'' :: Huffman -> Char -> [Bool] -> [Bool]
encode'' (Leaf _ c1) c bs = if c == c1 then bs else []
encode'' (Node _ h1 h2) c bs = encode'' h1 c (bs ++ [False]) ++ encode'' h2 c (bs ++ [True])

mkHuffmanTree :: String -> Huffman
mkHuffmanTree = huffmanTree . sort' . leafify

huffmanTree :: [Huffman] -> Huffman
huffmanTree [x]       = x
huffmanTree (x:(y:z)) = huffmanTree $ Node (val x + val y) x y : z

sort' :: [Huffman] -> [Huffman]
sort' = List.sortBy $ comparing val

leafify :: String -> [Huffman]
leafify s = fmap (\(c,i) -> Leaf i c) $ Map.toList $ toMap s

toMap :: String -> Map.Map Char Int
toMap = foldl (\a c -> Map.insertWith (+) c 1 a) (Map.empty :: Map.Map Char Int)

val :: Huffman -> Int
val (Leaf i _)   = i
val (Node i _ _) = i
