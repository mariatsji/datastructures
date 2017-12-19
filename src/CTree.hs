module CTree
    ( CTree(..),
      Position(..)
    ) where

data Position = Position deriving (Show, Ord, Eq)

data CTree = CNode Position Float [CTree] | WhiteWins | BlackWins | Remis deriving (Show, Ord, Eq)
