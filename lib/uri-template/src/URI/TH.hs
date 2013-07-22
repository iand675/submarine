{-# LANGUAGE TemplateHaskell #-}
module URI.TH where
import Data.List
import Text.Parsec.Prim
import Text.Parsec.String
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Network.HTTP.Base
import URI.Parser
import URI.Template
import URI.Types

{-
  mTpl <- parse qqInput
  case mTpl of
    Left err -> error err
    Right tpl -> do
      varNames <- distinct <$> getAllVariableNames
      convert varNames to [(varName, toTemplateValue $ varExpr for varName)]
      render tpl convertedValues
-}

variableNames :: UriTemplate -> [String]
variableNames = nub . foldr go []
  where
    go (Literal _) l = l
    go (Embed m vs) l = map variableName vs ++ l

templateToExp :: UriTemplate -> Q Exp
templateToExp ts = 

-- AppE (VarE 'concat) $ ListE $ concatMap segmentToExp ts

{-segmentToExp (Literal s) = [LitE $ StringL s]-}
{-segmentToExp (Embed m v) = map (AppE prefix . enc . VarE . mkName) v-}
	{-where-}
		{-enc = AppE (VarE $ encoder m)-}
		{--- cons the prefix onto the beginning of each embedded segment-}
		{-prefix = InfixE (Just $ LitE $ CharL $ subsequentSeparator m) (ConE $ '(:)) Nothing-}

quasiEval :: String -> Q Exp
quasiEval str = do
  l <- location
  let parseLoc = loc_module l ++ ":" ++ show (loc_start l)
  let res = parse uriTemplate parseLoc str
  case res of
    Left err -> fail $ show err
    Right tpl -> templateToExp tpl

uri :: QuasiQuoter
uri = QuasiQuoter
  { quoteExp = quasiEval
  , quotePat = error "Cannot use uri quasiquoter in pattern"
  , quoteType = error "Cannot use uri quasiquoter in type"
  , quoteDec = error "Cannot use uri quasiquoter as declarations"
	}
