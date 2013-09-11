module Submarine.Data.Util where

single :: [a] -> Maybe a
single xs = case xs of
	(x:[]) -> Just x
	_      -> Nothing
