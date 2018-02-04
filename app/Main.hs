module Main where

import Control.Parallel
import Control.DeepSeq

cool :: (Ord a, Enum a, NFData a, Ord b, Enum b, NFData b) => [(a,b)] -> [(a,b)]
cool zipped = par (force firsts) (pseq (force seconds) (firsts `zip` seconds))
    where firsts = fmap (succ . fst) zipped
          seconds = fmap (succ . snd) zipped


main :: IO ()
main = print $ cool $ ['o', 't', 's'] `zip` ([5, 2, 3] :: [Int])
