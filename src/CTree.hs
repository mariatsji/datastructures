module CTree
    ( CTree(..)
    ) where

data CTree a b = CNode a [CTree a b] | CNull b deriving (Show, Ord, Eq)

