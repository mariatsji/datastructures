module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM

funThreads :: IO ()
funThreads = do
   threadId <- forkIO (do
     tId <- myThreadId
     forever (do
       putStrLn $ "This is in first thread " ++ show tId))
   tId <- myThreadId
   forever (putStrLn $ "This is in second thread " ++ show tId)


main :: IO ()
main = do
  let s = newTVar 0 :: STM (TVar Int)
  forkIO (do
    iRef <- atomically s
    let m = modifyTVar' iRef succ
    print "subthread succ")
  tvar <- atomically s
  i <- readTVarIO tvar
  print i


