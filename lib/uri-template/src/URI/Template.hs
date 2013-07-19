module URI.Template where
import Data.List
import Data.Monoid
import URI.Types

data Allow = Unreserved | UnreservedOrReserved

data ProcessingOptions = ProcessingOptions
  { modifierPrefix :: Maybe Char
  , modifierSeparator :: Char
  , modifierSupportsNamed :: Bool
  , modifierIfEmpty :: Maybe Char
  , modifierAllow :: Allow
  }

option :: Maybe Char -> Char -> Bool -> Maybe Char -> Allow -> ProcessingOptions
option = ProcessingOptions

options :: Modifier -> ProcessingOptions
options m = case m of
  Simple            -> option Nothing    ',' False Nothing    Unreserved
  Reserved          -> option Nothing    ',' False Nothing    UnreservedOrReserved
  Label             -> option (Just '.') '.' False Nothing    Unreserved
  PathSegment       -> option (Just '/') '/' False Nothing    Unreserved
  PathParameter     -> option (Just ';') ';' True  Nothing    Unreserved
  Query             -> option (Just '?') '&' True  (Just '=') Unreserved
  QueryContinuation -> option (Just '&') '&' True  (Just '=') Unreserved
  Fragment          -> option (Just '#') ',' False Nothing    UnreservedOrReserved

processVariable :: Modifier -> Bool -> Variable -> TemplateValue -> String
processVariable m isFirst (Variable varName varMod) val = prefix encodedVariable
  where
    settings = options m
    encodedVariable = case val of
      (Single s) -> processSingle	varName s -- addStr varName >> addIfEmpIfEmptyString else addEqualSign >> processLengthVarMod >> appendProcessedString
      (Associative l) -> processAssociative varName l
      (List l) -> processList varName l
    prefix = if isFirst
      then maybe id (:) (modifierPrefix settings)
      else ((modifierSeparator settings) :)
    -- TODO: check s to handle emptiness and use the appropriate modifierIfEmpty setting
    -- TODO: handle correct encoding for interpolated values
    -- TODO: this is just gross.
    -- TODO: handle explodes / length issues
    processSingle vn s = if modifierSupportsNamed settings
      then vn ++ ('=' : s)
      else s
    processAssociative vn l = if modifierSupportsNamed settings
      then vn ++ "," ++ (intercalate "," $ foldr (\(l, r) -> (l :) . (r :)) [] l)
      else intercalate "," $ foldr (\(l, r) -> (l :) . (r :)) [] l
    processList vn l = if modifierSupportsNamed settings
      then vn ++ ('=' : intercalate "," l)
      else intercalate "," l

processVariables :: [(String, TemplateValue)] -> Modifier -> [Variable] -> [String]
processVariables env m vs = foldr go [] vs
  where
	  go = undefined

render :: UriTemplate -> [(String, TemplateValue)] -> String
render tpl env = concat $ foldr go [] tpl
  where
    renderWithEnv = processVariables env
    go :: TemplateSegment -> [String] -> [String]
    go (Literal s)  t = s : t
    go (Embed m vs) t = renderWithEnv m vs ++ t

{-
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

prefixAndSeparators :: Modifier -> [String]
prefixAndSeparators m = prefix m : separators m

separators :: Modifier -> [String]
separators m = repeat $ subsequentSeparator m

applyPrefixes :: [String] -> [String] -> [String]
applyPrefixes = zipWith (<>)

expandVariable (Variable varName varModifier) = undefined


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
-}
