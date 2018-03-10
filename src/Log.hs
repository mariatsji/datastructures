module Log
  ( Log(..)
  ) where

data Log
  = Cons String
         Log
  | LogEmpty
  deriving (Eq, Ord, Show)

mush :: Log -> Log -> Log
mush LogEmpty l1                = l1
mush (Cons sx LogEmpty) l1      = Cons sx l1
mush (Cons sx (Cons sxs l1)) l2 = Cons sx (Cons sxs (l1 `mush` l2))

instance Monoid Log where
  mappend = mush
  mempty = LogEmpty
