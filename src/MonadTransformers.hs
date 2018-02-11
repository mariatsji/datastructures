{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadTransformers (run) where

import Control.Monad.Trans
import Control.Monad.Trans.State as TS
import Control.Monad.Trans.Writer

import Control.Monad.State as S

-- StateT
stateFun :: State String String
stateFun = S.state (\s -> ("yo", s))

test :: State Int Int
test = do
  S.put 3
  S.modify (+1)
  S.get

run :: IO ()
run = print $ S.execState test 0

-- MonadTransformers


newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO ) a } deriving (Functor, Applicative, Monad)

foo :: Stack ()
foo = Stack $ do
  TS.put 1                 -- State layer
  lift $ tell [2]       -- Writer layer
  lift $ lift $ print 3 -- IO layer
  return ()

evalStack :: Stack a -> IO [Int]
evalStack m = execWriterT (evalStateT (unStack m) 0)
