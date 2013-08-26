module Submarine.JSON where
import Data.Aeson.TH
import Data.Char
import Data.List
import Language.Haskell.TH.Syntax

dropPrefix :: Name -> (String -> String)
dropPrefix n =  decapitalize . (drop $ length $ nameBase n)

decapitalize :: String -> String
decapitalize [] = []
decapitalize (x:xs) = toLower x : xs

snakeCase :: String -> String
snakeCase str = intercalate "_" $ map decapitalize $ chunk str []
	where
		chunk [] chunked = chunked
		chunk str chunked = let (match, rest) = break isUpper str in chunk rest (match : chunked)

jsonize :: Name -> Q [Dec]
jsonize name = deriveJSON (dropPrefix name) name