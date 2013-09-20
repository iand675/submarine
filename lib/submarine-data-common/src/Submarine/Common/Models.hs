{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Submarine.Common.Models (
  Identity(..),
  Id(..),
  Entity(..)
) where
import Control.Monad
import Data.Aeson
import Data.Functor.Identity
import Data.HashMap.Strict (insert)
import Data.Monoid ((<>))
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

type family Id a :: *

data Entity a = Entity
  { entityKey :: !(Id a)
  , entityValue :: !a
  }

instance (ToJSON a, ToJSON (Id a)) => ToJSON (Entity a) where
	toJSON e = Object (insert "id" (toJSON $ entityKey e) o)
		where (Object o) = toJSON $ entityValue e

instance Parsable UUID where
	parseParam p = case fromString $ unpack p of
		Nothing -> Left "Invalid UUID"
		Just u -> Right u

instance Parsable Text where
	parseParam = fmap toStrict . parseParam
