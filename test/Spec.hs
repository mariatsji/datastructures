import Test.Hspec
import Control.Exception (evaluate)

import CTree

data Result = WhiteWins | BlackWins deriving (Show, Ord, Eq, Bounded, Enum)
data Position = Position deriving (Show, Ord, Eq)

main :: IO ()
main = hspec $ do
  describe "CTree" $ do
    it "exposes a Data Constructor for CTree" $ do
      let c = (CNode (Position, 3.0) [] :: CTree (Position, Float) Result)
      print c