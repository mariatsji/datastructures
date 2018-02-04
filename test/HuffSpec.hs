import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.Map.Strict as Map

import Huffman

main :: IO ()
main = hspec $ do
  describe "Huffman" $ do
    it "can create a leaf huffman" $ do
      let leaf = Leaf 3 'c'
      val leaf `shouldBe` 3
    it "creates a map of chars with occurrences" $ do
      (toMap "aab" Map.! 'a') `shouldBe` 2
      (toMap "aab" Map.! 'b') `shouldBe` 1
    it "encodes a String" $ do
      let e = encode "aab"
      e `shouldBe` (Enc (Node 3 (Leaf 1 'b') (Leaf 2 'a'),[True,True,False]))
    it "encodes a little longer String" $ do
      let e = encode "alfalfa"
      print e
