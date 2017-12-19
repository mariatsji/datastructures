module Maze(Row, Col, Point, Path, Maze, Paths(..), start, end, search) where

type Row = Int
type Col = Int
type Point = (Row, Col)
type Path = [Point]
type Maze = [String]
data Paths = MazeNode Point Paths Paths Paths Paths | Deadend Point deriving (Show, Eq, Ord)

start :: Point
start = (2,0)

end :: Point
end = (2,4)

right :: Maze -> Path -> Point
right m p = let (c,r) = last p in (c + 1, r)

down :: Maze -> Path -> Point
down m p = let (c,r) = last p in (c, r + 1)

left :: Maze -> Path -> Point
left m p = let (c,r) = last p in (c - 1, r)

up :: Maze -> Path -> Point
up m p = let (c,r) =  last p in (c, r - 1)

search :: Maze -> Path -> Paths
search m p = MazeNode (last p) (searchRight m p) (searchDown m p) (searchLeft m p) (searchUp m p)

searchRight :: Maze -> Path -> Paths
searchRight m p
  | insideMaze m (right m p) && isEmpty m (right m p) = MazeNode (right m p) (searchRight m (p ++ [(right m p)])) (searchDown m p) (searchLeft m p) (searchUp m p)
  | otherwise = Deadend (right m p)

searchDown :: Maze -> Path -> Paths
searchDown m p
  | insideMaze m (down m p) && isEmpty m (down m p) = MazeNode (down m p) (searchRight m p) (searchDown m (p ++ [(down m p)])) (searchLeft m p) (searchUp m p)
  | otherwise = Deadend (down m p)

searchLeft :: Maze -> Path -> Paths
searchLeft m p
  | insideMaze m (left m p) && isEmpty m (left m p) = MazeNode (left m p) (searchRight m p) (searchDown m p) (searchLeft m (p ++ [(left m p)])) (searchUp m p)
  | otherwise = Deadend (left m p)

searchUp :: Maze -> Path -> Paths
searchUp m p
  | insideMaze m (up m p) && isEmpty m (up m p) = MazeNode (up m p) (searchRight m p) (searchDown m p) (searchLeft m p) (searchUp m (p ++ [(up m p)]))
  | otherwise = Deadend (up m p)

isEmpty :: Maze -> Point -> Bool
isEmpty maze (r,c) = maze !! r !! c == ' '
isEmpty _ _ = False

insideMaze :: Maze -> Point -> Bool
insideMaze maze (r,c) = r < length maze && r >= 0 && c < length (maze !! r) && c >= 0