module Stateful
  (
  ) where

import           Control.Monad.State

get' :: State Int Int
get' = get

put' :: Int -> State Int ()
put' i = State $ (\i -> ((), i))
