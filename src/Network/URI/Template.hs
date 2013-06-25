module Network.URI.Template where
import Control.Applicative
import Data.Char
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.Text
import Text.Parsec.Token

type Variable = String

data TemplateSegment
	= Literal String
	| Embed Modifier [Variable]
	deriving (Read, Show, Eq)

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
		[ ('+', Reserved), ('#', Fragment), ('.', Label)
		, ('/', PathSegment), (';', PathParameter)
		, ('?', Query), ('&', QueryContinuation)
		]

variable :: Parser Variable
variable = concat <$> many1 ((pure <$> (alphaNum <|> char '_')) <|> pctEncoded)

embed :: Parser TemplateSegment
embed = between (char '{') (char '}') variables

urlTemplate :: Parser [TemplateSegment]
urlTemplate = many (literal <|> embed)
