{-# LANGUAGE DefaultSignatures, FlexibleInstances #-}
module Network.URI.Template where
import Control.Applicative
import Data.Char
import Data.List
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec.Token

data TemplateValue
	= Single String
	| Associative [(String, String)]
	| List [String]
	deriving (Read, Show, Eq)

class ToTemplateValue a where
	toTemplateValue :: a -> TemplateValue

instance ToTemplateValue Int where
	toTemplateValue = Single . show

prefix :: Modifier -> String
prefix m = case m of
	Simple -> ""
	Reserved -> ""
	Fragment -> "#"
	Label -> "."
	PathSegment -> "/"
	PathParameter -> ";"
	Query -> "?"
	QueryContinuation -> "&"

subsequentSeparator :: Modifier -> String
subsequentSeparator m = case m of
	Simple -> ","
	Reserved -> ","
	Fragment -> ","
	Label -> "."
	PathSegment -> "/"
	PathParameter -> ";"
	Query -> "&"
	QueryContinuation -> "&"


data ValueModifier = Normal | Explode | MaxLength Int
	deriving (Read, Show, Eq)

data Variable = Variable String ValueModifier
	deriving (Read, Show, Eq)

data TemplateSegment
	= Literal String
	| Embed Modifier [Variable]
	deriving (Read, Show, Eq)

type UriTemplate = [TemplateSegment]

data Modifier = Simple | Reserved | Fragment | Label | PathSegment | PathParameter | Query | QueryContinuation
	deriving (Read, Show, Eq)

range :: Char -> Char -> Parser Char
range l r = satisfy (\c -> l <= c && c <= r)

ranges :: [(Char, Char)] -> Parser Char
ranges = choice . map (uncurry range)

ucschar :: Parser Char
ucschar = ranges
	[ ('\xA0', '\xD7FF')
	, ('\xF900', '\xFDCF')
	, ('\xFDF0', '\xFFEF')
	, ('\x10000', '\x1FFFD')
	, ('\x20000', '\x2FFFD')
	, ('\x30000', '\x3FFFD')
	, ('\x40000', '\x4FFFD')
	, ('\x50000', '\x5FFFD')
	, ('\x60000', '\x6FFFD')
	, ('\x70000', '\x7FFFD')
	, ('\x80000', '\x8FFFD')
	, ('\x90000', '\x9FFFD')
	, ('\xA0000', '\xAFFFD')
	, ('\xB0000', '\xBFFFD')
	, ('\xC0000', '\xCFFFD')
	, ('\xD0000', '\xDFFFD')
	, ('\xE1000', '\xEFFFD')
	]

iprivate :: Parser Char
iprivate = ranges
	[ ('\xE000', '\xF8FF')
	, ('\xF0000', '\xFFFFD')
	, ('\x100000', '\x10FFFD')
	]

pctEncoded :: Parser String
pctEncoded = do
	h <- char '%'
	d1 <- hexDigit
	d2 <- hexDigit
	return [h, d1, d2]

literalChar :: Parser Char
literalChar = (choice $ map char ['\x21', '\x23', '\x24', '\x26', '\x3D', '\x5D', '\x5F', '\x7E'])
	<|> ranges [('\x28', '\x3B'), ('\x3F', '\x5B'), ('\x61', '\x7A')]
	<|> ucschar
	<|> iprivate

literal :: Parser TemplateSegment
literal = (Literal . concat) <$> many1 ((pure <$> literalChar) <|> pctEncoded)

variables :: Parser TemplateSegment
variables = Embed <$> modifier <*> sepBy1 variable (spaces *> char ',' *> spaces)

means :: Parser a -> b -> Parser b
means p v = p *> pure v

charMeans = means . char

modifier :: Parser Modifier
modifier = (choice $ map (uncurry charMeans) modifiers) <|> pure Simple
	where modifiers =
		[ ('+', Reserved)
    , ('#', Fragment)
    , ('.', Label)
		, ('/', PathSegment)
    , (';', PathParameter)
		, ('?', Query)
    , ('&', QueryContinuation)
		]

variable :: Parser Variable
variable = Variable <$> name <*> valueModifier
	where
		name = concat <$> many1 ((pure <$> (alphaNum <|> char '_')) <|> pctEncoded)
		valueModifier = charMeans '*' Explode <|> (MaxLength <$> (char ':' *> parseInt)) <|> pure Normal
		parseInt = read <$> many1 digit

embed :: Parser TemplateSegment
embed = between (char '{') (char '}') variables

uriTemplate :: Parser UriTemplate
uriTemplate = spaces *> many (literal <|> embed)

renderTemplate :: UriTemplate -> [(String, TemplateValue)] -> Maybe String
renderTemplate u vs = do
	sections <- mapM (stringify vs) u
	return $ concat sections

stringify :: [(String, TemplateValue)] -> TemplateSegment -> Maybe String
stringify varMap templateSection = case templateSection of
	Literal l -> Just l
	Embed m vars -> do
		rs <- mapM (something m) vars
		return $ prefix m ++ intercalate (subsequentSeparator m) rs
	where
		something :: Modifier -> Variable -> Maybe String
		something m (Variable varName valueModifier) = (stringifyTemplateValue m varName) <$> lookup varName varMap

stringifyTemplateValue :: Modifier -> String -> TemplateValue -> String
stringifyTemplateValue m name t = case t of
	(Single s) -> if (m == Query || m == QueryContinuation || m == PathParameter) then name ++ "=" ++ s else s
	(Associative ss) -> ""
	(List ss) -> ""
