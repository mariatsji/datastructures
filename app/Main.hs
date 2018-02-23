module Main where

import qualified MonadTransformers as MT

main :: IO ()
main = do
  ints <- MT.read' $ MT.trav ["hello", "bananas", "this", "works"]
  mapM_ print ints


