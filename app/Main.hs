{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import           System.Directory
import           System.FilePath

newtype AppConfig = AppConfig { cfgMaxDepth :: Int
} deriving (Show)

newtype AppState = AppState { stDeepestReached :: Int
} deriving (Show)

newtype MyApp a = MyA {
    runA :: StateT AppState (ReaderT AppConfig IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig,
  MonadState AppState)

runApp :: MyApp a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runReaderT (runStateT (runA k) state) config

constrainedCount :: Int -> FilePath -> MyApp [(FilePath, Int)]
constrainedCount curDepth path = do
    contents <- liftIO . listDirectory $ path
    cfg <- ask
    rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
                then do
                    let newDepth = curDepth + 1
                    st <- get
                    when (stDeepestReached st < newDepth) $
                        put st { stDeepestReached = newDepth }
                    constrainedCount newDepth newPath
                else return []
    return $ (path, length contents) : concat rest

main = return ()