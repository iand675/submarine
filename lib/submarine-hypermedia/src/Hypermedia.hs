{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.API.Hypermedia where
import Control.Lens.TH
import Control.Monad.State.Strict
import Data.Aeson.Types (
	Object,
	Value,
	FromJSON,
	parseJSON,
	Value(String),
	ToJSON,
	toJSON)
import Data.Aeson.TH
import Data.Default
import Data.HashMap.Strict (HashMap)

data InputType
	= Hidden
	| Text
	| Search
	| Telephone
	| URL
	| Email
	| Password
	| DateTime
	| Date
	| Month
	| Week
	| Time
	| DateTimeLocal
	| Number
	| Range
	| Color
	| Checkbox
	| Radio
	| File
	| Submit
	| Image
	| Reset
	| Button
	deriving (Show, Eq)

instance ToJSON InputType where
	toJSON Hidden = "hidden"
	toJSON Text = "text"
	toJSON Search = "search"
	toJSON Telephone = "telephone"
	toJSON URL = "url"
	toJSON Email = "email"
	toJSON Password = "password"
	toJSON DateTime = "datetime"
	toJSON Date = "date"
	toJSON Month = "month"
	toJSON Week = "week"
	toJSON Time = "time"
	toJSON DateTimeLocal = "datetime-local"
	toJSON Number = "number"
	toJSON Range = "range"
	toJSON Color = "color"
	toJSON Checkbox = "checkbox"
	toJSON Radio = "radio"
	toJSON File = "file"
	toJSON Submit = "submit"
	toJSON Image = "image"
	toJSON Reset = "reset"
	toJSON Button = "button"

instance FromJSON InputType where
	parseJSON (String "hidden") = return Hidden
	parseJSON (String "text") = return Text
	parseJSON (String "search") = return Search
	parseJSON (String "telephone") = return Telephone
	parseJSON (String "url") = return URL
	parseJSON (String "email") = return Email
	parseJSON (String "password") = return Password
	parseJSON (String "datetime") = return DateTime
	parseJSON (String "date") = return Date
	parseJSON (String "month") = return Month
	parseJSON (String "week") = return Week
	parseJSON (String "time") = return Time
	parseJSON (String "datetime-local") = return DateTimeLocal
	parseJSON (String "number") = return Number
	parseJSON (String "range") = return Range
	parseJSON (String "color") = return Color
	parseJSON (String "checkbox") = return Checkbox
	parseJSON (String "radio") = return Radio
	parseJSON (String "file") = return File
	parseJSON (String "submit") = return Submit
	parseJSON (String "image") = return Image
	parseJSON (String "reset") = return Reset
	parseJSON (String "button") = return Button
	parseJSON _ = fail "input type must be string"

data Field = Field
	{ _fieldName :: String
	, _fieldType :: Maybe InputType
	, _fieldValue :: Maybe Value
	} deriving (Show, Eq)

data Action = Action
	{ _actionName :: String
	, _actionClassVal :: Maybe [String]
	, _actionMethod :: Maybe String
	, _actionHref :: String
	, _actionTitle :: Maybe String
	, _actionType :: Maybe String
	, _actionFields :: Maybe [Field]
	} deriving (Show, Eq)

data Link = Link
	{ _linkRel :: [String]
	, _linkHref :: String
	} deriving (Show, Eq)

data Entity = Entity
	{ _entityClassVal :: Maybe [String]
	, _entityProperties :: Maybe Object
	, _entityEntities :: Maybe [Entity]
	, _entityActions :: Maybe [Action]
	, _entityLinks :: Maybe [Link]
	} deriving (Show, Eq)

deriveJSON id ''Field
deriveJSON id ''Action
deriveJSON id ''Link
deriveJSON id ''Entity

class ToEntity a where
	toEntity :: a -> Entity

makeFields ''Field
makeFields ''Action
makeFields ''Link
makeFields ''Entity

instance Default Entity where
	def = Entity def def def def def

entity :: State Entity a -> Entity
entity = flip execState $ def
