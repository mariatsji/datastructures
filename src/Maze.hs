module Maze(Row, Col, Point, Path, Maze, Paths(..), entrance, exit, search, solution) where

type Row = Int
type Col = Int
type Point = (Row, Col)
type Path = [Point]
type Maze = [String]
data Paths = MazeNode Point Paths Paths Paths Paths | Deadend Point | Exit Point deriving (Show, Eq, Ord)

entrance :: Point
entrance = (2,0)

exit :: Point
exit = (2,4)

right :: Maze -> Path -> Point
right m p = let (c,r) = last p in (c + 1, r)

down :: Maze -> Path -> Point
down m p = let (c,r) = last p in (c, r + 1)

left :: Maze -> Path -> Point
left m p = let (c,r) = last p in (c - 1, r)

up :: Maze -> Path -> Point
up m p = let (c,r) =  last p in (c, r - 1)

search :: Maze -> Path -> Paths
search m p = MazeNode (last p) (search' m (p ++ [right m p])) (search' m (p ++ [down m p])) (search' m (p ++ [left m p])) (search' m (p ++ [up m p]))

search' :: Maze -> Path -> Paths
search' m p
  | foundExit m p = Exit (last p)
  | hasBeenThere m p = Deadend (last p)
  | insideMaze m (last p) && isEmpty m (last p) = search m p
  | otherwise = Deadend (last p)

solution :: Paths -> Path -> Path
solution (Exit n) p = [n]
solution (MazeNode n r d l u) p
  | [] /= solution r p = [n] ++ solution r p
  | [] /= solution d p = [n] ++ solution d p
  | [] /= solution l p = [n] ++ solution l p
  | [] /= solution u p = [n] ++ solution u p
  | otherwise = []
solution (Deadend _)  p = []

hasBeenThere :: Maze -> Path -> Bool
hasBeenThere m path = (last path) `elem` (init path)

foundExit :: Maze -> Path -> Bool
foundExit m path = let (c,r) = (last path) in (c,r) == exit

isEmpty :: Maze -> Point -> Bool
isEmpty maze (r,c) = maze !! r !! c == ' '

insideMaze :: Maze -> Point -> Bool
insideMaze maze (r,c) = r < length maze && r >= 0 && c < length (maze !! r) && c >= 0