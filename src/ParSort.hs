module ParSort where

import Control.Parallel (par, pseq)
import Control.DeepSeq (force, NFData(..))

parSort :: (Ord a, NFData a) => [a] -> [a]
parSort (x:xs)    = force greater `par` (force lesser `pseq`
                                         (lesser ++ x:greater))
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _       = []