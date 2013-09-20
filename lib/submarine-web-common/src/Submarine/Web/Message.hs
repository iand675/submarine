{-# LANGUAGE OverloadedStrings #-}
module Submarine.Web.Message where
import Data.Aeson
import Data.Text
import Submarine.Errors

data Message a = Message
	{ messageMessage :: Text
	, messageDetails :: a
	}

instance ToJSON a => ToJSON (Message a) where
	toJSON (Message m d) = object ["message" .= m, "details" .= d]

instance ToJSON SingleValueError where
	toJSON t = toJSON $ Message "service expected single value to be returned" (show t)
