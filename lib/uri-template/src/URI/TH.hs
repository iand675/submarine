{-# LANGUAGE TemplateHaskell #-}
module URI.TH where
import Text.Parsec.Prim
import Text.Parsec.String
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Network.HTTP.Base
import URI.Template

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

separators = (prefix m) : repeat (subsequentSeparator m)



templateToExp :: UriTemplate -> Exp
templateToExp ts = AppE (VarE 'concat) $ ListE $ concatMap segmentToExp ts

segmentToExp (Literal s) = [LitE $ StringL s]
segmentToExp (Embed m v) = map (AppE prefix . enc . VarE . mkName) v
	where
		enc = AppE (VarE $ encoder m)
		-- cons the prefix onto the beginning of each embedded segment
		prefix = InfixE (Just $ LitE $ CharL $ subsequentSeparator m) (ConE $ '(:)) Nothing

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
