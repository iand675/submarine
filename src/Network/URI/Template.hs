{-# LANGUAGE TemplateHaskell #-}
module Network.URI.Template where
import Control.Applicative
import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Network.HTTP.Base
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec.Token

type Variable = String

data TemplateSegment
	= Literal String
	| Embed Modifier [Variable]
	deriving (Read, Show, Eq)

type UriTemplate = [TemplateSegment]

data Modifier = Simple | Reserved | Fragment | Label | PathSegment | PathParameter | Query | QueryContinuation
	deriving (Read, Show, Eq)

separator :: Modifier -> Char
separator m = case m of
	Simple -> ','
	Reserved -> ','
	Fragment -> ','
	Label -> '.'
	PathSegment -> '/'
	PathParameter -> ';'
	Query -> '&'
	QueryContinuation -> '&'

encoder :: Modifier -> Name
encoder m = case m of
	Simple -> 'urlEncode
	Reserved -> 'id
	Fragment -> 'id
	Label -> 'id
	PathSegment -> 'urlEncode
	PathParameter -> 'urlEncode
	Query -> 'id
	QueryContinuation -> 'id

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

uriTemplate :: Parser UriTemplate
uriTemplate = spaces *> many (literal <|> embed)

templateToExp :: UriTemplate -> Exp
templateToExp ts = AppE (VarE 'concat) $ ListE $ concatMap segmentToExp ts

segmentToExp (Literal s) = [LitE $ StringL s]
segmentToExp (Embed m v) = map (AppE prefix . enc . VarE . mkName) v
	where
		enc = AppE (VarE $ encoder m)
		-- cons the prefix onto the beginning of each embedded segment
		prefix = InfixE (Just $ LitE $ CharL $ separator m) (ConE $ '(:)) Nothing

quasiEval :: String -> Q Exp
quasiEval str = do
	l <- location
	let parseLoc = loc_module l ++ ":" ++ show (loc_start l)
	let res = parse uriTemplate parseLoc str
	case res of
		Left err -> fail $ show err
		Right tpl -> return $ templateToExp tpl

uri :: QuasiQuoter
uri = QuasiQuoter
	{ quoteExp = quasiEval
	, quotePat = error "Cannot use uri quasiquoter in pattern"
	, quoteType = error "Cannot use uri quasiquoter in type"
	, quoteDec = error "Cannot use uri quasiquoter as declarations"
	}
