import Test.Hspec
import Control.Exception (evaluate)

import MonadTransformers

main :: IO ()
main = hspec $ do
  describe "MonadTransformers" $ do
    it "dummy tests" $ do
      MonadTransformers.run
