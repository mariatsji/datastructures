import Test.Hspec
import Control.Exception (evaluate)

import Data.Monoid
import Log

main :: IO ()
main = hspec $ do
  describe "Log" $ do
    it "can append two longer logs" $ do
        let log1 = (Cons "yo" (Cons "dude" (Cons "this" LogEmpty)))
        let log2 = (Cons "is" (Cons "rather" (Cons "awesome" LogEmpty)))
        log1 <> log2 `shouldBe` (Cons "yo" (Cons "dude" (Cons "this" (Cons "is" (Cons "rather" (Cons "awesome" LogEmpty))))))
