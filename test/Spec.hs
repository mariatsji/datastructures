import Test.Hspec
import Control.Exception (evaluate)

import Maze as M


maze1 =[
   "#####"
  ,"#   #"
  ,"  #  "
  ,"### #"
  ,"#####"]

main :: IO ()
main = hspec $ do
  describe "Maze" $ do
    it "exposes a Maze to be constructed" $ do
      let m = MazeNode (3,3) (Deadend (3,4)) (Deadend (4,3)) (Deadend (2,3)) (Deadend (3,2))
      print "yo"
      -- print m
    it "searches a maze " $ do
      let s = M.search maze1 [M.start]
      print s
