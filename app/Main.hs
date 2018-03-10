{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.Writer

data AppConfig =
  AppConfig
  deriving (Show)

type App a = WriterT [(Int, String)] (ReaderT AppConfig IO) a

zeList = [5, 26, 4]

visit :: Int -> [(Int, String)]
visit i = [(i, "Visited [" ++ show i ++ "]")]

processList :: [Int] -> App [()]
processList = mapM (tell . visit)

main :: IO ()
main = do
  let res = runWriterT $ processList zeList
  let oo = runReaderT res AppConfig
  hmm <- liftIO oo
  print hmm
