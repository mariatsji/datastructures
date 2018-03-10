module WriterFun where

import           Control.Monad.Writer (Writer, runWriter, tell)

type App = Writer [String] Int

max' :: Int -> [Int] -> App
max' c [] = do
  tell ["Not much more to tell, really"]
  pure 0 -- w00t? why does this work?
max' c (x:xs)
  | c >= x = tell ["retaining max " ++ show c] >> max' c xs
  | otherwise = tell ["found new max " ++ show x] >> max' x xs
