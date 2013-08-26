module Submarine.Common.Models where
import Control.Monad
import Data.Aeson
import Data.Functor.Identity
import Data.UUID

instance (ToJSON a) => ToJSON (Identity a) where
  toJSON = toJSON . runIdentity 

instance (FromJSON a) => FromJSON (Identity a) where
  parseJSON = fmap Identity . parseJSON

instance ToJSON UUID where
  toJSON = toJSON . toString

instance FromJSON UUID where
  parseJSON = do
  	str <- parseJSON
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
