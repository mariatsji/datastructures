module MonadTransformers
  ( trav
  , read'
  ) where

import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Writer (WriterT, execWriterT, tell)

-- WriterT w m a
-- StateT  s m a
type App = WriterT [Int] IO ()

type App2 = WriterT (StateT [Int] IO ())

type App3 = StateT (WriterT [Int] IO ())

len :: String -> IO Int
len = return . length

trav :: [String] -> App
trav l = do
  ls <- liftIO $ mapM len l
  tell ls

read' :: App -> IO [Int]
read' w = execWriterT w
