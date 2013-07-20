module URI.Template where
import Control.Monad.Writer.Strict
import Data.DList hiding (map)
import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import URI.Types

type StringBuilder = Writer (DList Char)

addChar :: Char -> StringBuilder ()
addChar = tell . singleton

addString :: String -> StringBuilder ()
addString = tell . fromList

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

templateValueIsEmpty (Single s) = null s
templateValueIsEmpty (Associative s) = null s
templateValueIsEmpty (List s) = null s

namePrefix :: ProcessingOptions -> String -> TemplateValue -> StringBuilder ()
namePrefix opts name val = do
  addString name
  if templateValueIsEmpty val
    then maybe (return ()) addChar $ modifierIfEmpty opts
    else addChar '='

processVariable :: Modifier -> Bool -> Variable -> TemplateValue -> StringBuilder ()
processVariable m isFirst (Variable varName varMod) val = do
  if isFirst
    then maybe (addChar $ modifierSeparator settings) addChar $ modifierPrefix settings
    else addChar $ modifierSeparator settings
  if varMod /= Explode
    then do
      when (modifierSupportsNamed settings) (namePrefix settings varName val)
      unexploded
    else exploded
  where
    settings = options m
    sepByCommas = sequence_ . intersperse (addChar ',')
    associativeCommas (n, v) = addString n >> addChar ',' >> addString v
    unexploded = case val of
      (Single s) -> addString s
      (Associative l) -> sepByCommas $ map associativeCommas l
      (List l) -> sepByCommas $ map addString l
    explodedAssociative (k, v) = do
      addString k
      addChar '='
      addString v
    exploded = case val of
      (Single s) -> do
        when (modifierSupportsNamed settings) (namePrefix settings varName val)
        addString s
      (Associative l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map explodedAssociative l
      (List l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map addString l

processVariables env m vs = undefined
  where
    nonEmptyVariables :: [(Variable, TemplateValue)]
    nonEmptyVariables = catMaybes $ map (\v -> fmap (\mv -> (v, mv)) $ findValue v) vs
    findValue (Variable varName _) = lookup varName env

render :: UriTemplate -> [(String, TemplateValue)] -> String
render tpl env = toList $ execWriter $ undefined
  where
    go :: TemplateSegment -> StringBuilder ()
    go (Literal s) = addString s
    go (Embed m vs) = undefined
      {-(processVariable m True) : repeat (processVariable m False)-}
