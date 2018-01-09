import Test.Hspec
import Control.Exception (evaluate)

import Maze as M
import Log

maze1 =[
   "#####"
  ,"#   #"
  ,"  #  "
  ,"### #"
  ,"#####"]

maze2 = [
  "#####"
 ,"# ###"
 ,"  ## "
 ,"# ## "
 ,"#    "
  ]

main :: IO ()
main = hspec $ do
  describe "Maze" $ do
    it "exposes a Maze to be constructed" $ do
      let m = MazeNode (3,3) (Deadend (3,4)) (Deadend (4,3)) (Deadend (2,3)) (Deadend (3,2))
      print "yo"
      -- print m
    it "searches a maze " $ do
      let s = M.search maze1 [M.entrance]
      print s
    it "prints the path to the solved maze" $ do
      let s = M.search maze1 [M.entrance]
      let sol = M.solution s [M.entrance]
      print $ prettyMaze maze1
      print sol