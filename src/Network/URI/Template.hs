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

ucschar :: Parser Char
ucschar
	=   range '\xA0' '\xD7FF'
	<|> range '\xF900' '\xFDCF'
	<|> range '\xFDF0' '\xFFEF'
	<|> range '\x10000' '\x1FFFD'
	<|> range '\x20000' '\x2FFFD'
	<|> range '\x30000' '\x3FFFD'
	<|> range '\x40000' '\x4FFFD'
	<|> range '\x50000' '\x5FFFD'
	<|> range '\x60000' '\x6FFFD'
	<|> range '\x70000' '\x7FFFD'
	<|> range '\x80000' '\x8FFFD'
	<|> range '\x90000' '\x9FFFD'
	<|> range '\xA0000' '\xAFFFD'
	<|> range '\xB0000' '\xBFFFD'
	<|> range '\xC0000' '\xCFFFD'
	<|> range '\xD0000' '\xDFFFD'
	<|> range '\xE1000' '\xEFFFD'

iprivate :: Parser Char
iprivate
	=   range '\xE000' '\xF8FF'
	<|> range '\xF0000' '\xFFFFD'
	<|> range '\x100000' '\x10FFFD'

pctEncoded :: Parser String
pctEncoded = do
	h <- char '%'
	d1 <- hexDigit
	d2 <- hexDigit
	return [h, d1, d2]

literalChar :: Parser Char
literalChar
	=   (choice $ map char ['\x21', '\x23', '\x24', '\x26', '\x3D', '\x5D', '\x5F', '\x7E'])
	<|> range '\x28' '\x3B'
	<|> range '\x3F' '\x5B'
	<|> range '\x61' '\x7A'
	<|> ucschar
	<|> iprivate

literal :: Parser TemplateSegment
literal = (Literal . concat) <$> many1 ((pure <$> literalChar) <|> pctEncoded)

variables :: Parser TemplateSegment
variables = Embed <$> modifier <*> sepBy1 variable (spaces *> char ',' *> spaces)

modifier :: Parser Modifier
modifier
	=   (char '+' *> pure Reserved)
	<|> (char '#' *> pure Fragment)
	<|> (char '.' *> pure Label)
	<|> (char '/' *> pure PathSegment)
	<|> (char ';' *> pure PathParameter)
	<|> (char '?' *> pure Query)
	<|> (char '&' *> pure QueryContinuation)
	<|> pure Simple

variable :: Parser Variable
variable = concat <$> many1 ((pure <$> (alphaNum <|> char '_')) <|> pctEncoded)

embed :: Parser TemplateSegment
embed = between (char '{') (char '}') variables

urlTemplate :: Parser [TemplateSegment]
urlTemplate = many (literal <|> embed)
