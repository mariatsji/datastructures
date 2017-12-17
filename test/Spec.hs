import Test.Hspec
import Control.Exception (evaluate)

import CTree


main :: IO ()
main = hspec $ do
  describe "CTree" $ do
    it "exposes a Data Constructor for CTree" $ do
      let c = CNode Position 3.0 []
      print c
      let e = WhiteWins
      print e

