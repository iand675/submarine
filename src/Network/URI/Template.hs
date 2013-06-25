module Network.URI.Template where
import Control.Applicative
import Data.Char
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.Text

data TemplateSegment
	= Literal String
	| Embed [String]
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
	=   char  '\x21'
	<|> char  '\x23'
	<|> char  '\x24'
	<|> char  '\x26'
	<|> char  '\x3D'
	<|> char  '\x5D'
	<|> char  '\x5F'
	<|> char  '\x7E'
	<|> range '\x28' '\x3B'
	<|> range '\x3F' '\x5B'
	<|> range '\x61' '\x7A'
	<|> ucschar
	<|> iprivate

literal :: Parser TemplateSegment
literal = (Literal . concat) <$> many1 ((pure <$> literalChar) <|> pctEncoded)

variable :: Parser TemplateSegment
variable = (Embed . pure . concat) <$> many1 ((pure <$> (alphaNum <|> char '_')) <|> pctEncoded)

embed :: Parser TemplateSegment
embed = between (char '{') (char '}') variable
