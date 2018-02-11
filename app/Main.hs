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


inc :: Enum a => STM (TVar a) -> IO ()
inc s = do
  iRef <- atomically s
  let m = modifyTVar' iRef succ
  print "subthread succ"

read' :: Show a => STM (TVar a) -> IO ()
read' s = do
  tvar <- atomically s
  val <- readTVarIO tvar
  print val

main :: IO ()
main = do
  let stm = newTVar 0 :: STM (TVar Int)
  forkIO ( inc stm )
  forkIO ( read' stm )
  threadDelay 1000


