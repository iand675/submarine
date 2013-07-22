module URI.Types where

data TemplateValue
	= Single String
	| Associative [(String, String)]
	| List [String]
	deriving (Read, Show, Eq)

class ToTemplateValue a where
	toTemplateValue :: a -> TemplateValue

instance ToTemplateValue Int where
	toTemplateValue = Single . show

data ValueModifier
  = Normal
  | Explode
  | MaxLength Int
	deriving (Read, Show, Eq)

data Variable = Variable { variableName :: String, varaibleValueModifier :: ValueModifier }
	deriving (Read, Show, Eq)

data TemplateSegment
	= Literal String
	| Embed Modifier [Variable]
	deriving (Read, Show, Eq)

type UriTemplate = [TemplateSegment]

data Modifier
  = Simple
  | Reserved
  | Fragment
  | Label
  | PathSegment
  | PathParameter
  | Query
  | QueryContinuation
	deriving (Read, Show, Eq)


