{-# LANGUAGE OverloadedStrings #-}
module Submarine.Common.Models (
  Identity(..),
  Id(..),
  Entity(..)
) where
import Control.Monad
import Data.Aeson
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text.Lazy (unpack, toStrict)
import Data.UUID
import Web.Scotty (Parsable(..))

instance (ToJSON a) => ToJSON (Identity a) where
  toJSON = toJSON . runIdentity 

instance (FromJSON a) => FromJSON (Identity a) where
  parseJSON = fmap Identity . parseJSON

instance ToJSON UUID where
  toJSON = toJSON . toString

instance FromJSON UUID where
  parseJSON v = do
  	str <- parseJSON v
  	case fromString str of
  		Nothing -> fail "Invalid UUID"
  		Just u -> return u

instance ToJSON (Id a) where
  toJSON (Id u) = toJSON u

instance FromJSON (Id a) where
  parseJSON = fmap Id . parseJSON

data Entity a = Entity
  { entityKey :: !(Id a)
  , entityValue :: !a
  }

newtype Id a = Id UUID

instance Parsable UUID where
	parseParam p = case fromString $ unpack p of
		Nothing -> Left "Invalid UUID"
		Just u -> Right u

instance Parsable (Id a) where
	parseParam = fmap Id . parseParam

instance Parsable Text where
	parseParam = fmap toStrict . parseParam