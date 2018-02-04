module Research(carry) where

-- apply something, and pass it alongside result to next function
-- carry 1 (+1) (+) = 3
carry :: a -> (a -> b) -> (a -> b -> c) -> c
carry a f g = let b = f a in g a b