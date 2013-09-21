{-# LANGUAGE OverloadedStrings #-}
module Util where
import Data.Aeson(ToJSON, FromJSON)
import Network.HTTP.Types

import Config
import Submarine.Errors
import Submarine.Common.Models
import Submarine.Web.Actions
import Submarine.Web.Message

whenOk :: ToJSON a => (b -> Handler) -> Either a b -> Handler
whenOk _ (Left e) = status internalServerError500 >> json e
whenOk f (Right x) = f x

whenFound :: (a -> Handler) -> Maybe a -> Handler
whenFound _ Nothing = status notFound404 >> json (Message "item not found" ())
whenFound f (Just x) = f x

returnSingle :: (ToJSON a, ToJSON b) => Either a (Maybe b) -> Handler
returnSingle = whenOk (whenFound json)

