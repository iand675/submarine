{-# LANGUAGE EmptyDataDecls, GADTs, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module URI.Types where

data SingleElement
data AssociativeListElement
data ListElement

newtype ListElem a = ListElem { fromListElem :: a }

data TemplateValue a where
	Single :: String -> TemplateValue SingleElement
	Associative :: [(String, TemplateValue SingleElement)] -> TemplateValue AssociativeListElement
	List :: [TemplateValue SingleElement] -> TemplateValue ListElement

class ToTemplateValue a e | a -> e where
	toTemplateValue :: a -> TemplateValue e

instance ToTemplateValue Int SingleElement where
	toTemplateValue = Single . show

instance ToTemplateValue a SingleElement => ToTemplateValue (ListElem [a]) ListElement where
  toTemplateValue = List . map toTemplateValue . fromListElem

instance ToTemplateValue a SingleElement => ToTemplateValue [(String, a)] AssociativeListElement where
  toTemplateValue = Associative . map (\(l, r) -> (l, toTemplateValue r))

instance ToTemplateValue String SingleElement where
  toTemplateValue = Single

data ValueModifier
  = Normal
  | Explode
  | MaxLength Int
	deriving (Read, Show, Eq)

data Variable = Variable { variableName :: String, variableValueModifier :: ValueModifier }
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


