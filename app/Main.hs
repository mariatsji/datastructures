module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef

main :: IO ()
main = do
  ref <- newIORef 0 :: IO (IORef Int)
  forkIO ( modifyIORef' ref succ )
  threadDelay 100
  val <- readIORef ref
  print val

