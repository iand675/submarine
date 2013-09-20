module Submarine.Data.Util where
import Data.ByteString.Char8 (ByteString, pack)
import Data.UUID (toString)
import Submarine.Common.Models
import Submarine.Errors

exactlyOne :: [a] -> Either SingleValueError a
exactlyOne xs = case xs of
	[]     -> Left NotEnough
	(x:[]) -> Right x
	_      -> Left TooMany

oneOrNothing :: [a] -> Either SingleValueError (Maybe a)
oneOrNothing [] = Right Nothing
oneOrNothing (x:[]) = Right (Just x)
oneOrNothing _ = Left TooMany
