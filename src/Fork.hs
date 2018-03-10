module Fork where

data Fork a =
  Fork (Fork a)
       a
       (Fork a)
  deriving (Show)

instance Functor Fork where
  fmap f (Fork l m r) = fmap f l (f m) fmap f r
