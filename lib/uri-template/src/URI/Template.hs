{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables #-}
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

addSingle :: TemplateValue SingleElement -> StringBuilder ()
addSingle (Single s) = addString s

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

templateValueIsEmpty :: TemplateValue a -> Bool
templateValueIsEmpty (Single s) = null s
templateValueIsEmpty (Associative s) = null s
templateValueIsEmpty (List s) = null s

namePrefix :: ProcessingOptions -> String -> TemplateValue a -> StringBuilder ()
namePrefix opts name val = do
  addString name
  if templateValueIsEmpty val
    then maybe (return ()) addChar $ modifierIfEmpty opts
    else addChar '='

processVariable :: Modifier -> Bool -> Variable -> TemplateValue a -> StringBuilder ()
processVariable m isFirst (Variable varName varMod) val = do
  if isFirst
    then maybe (return ()) addChar $ modifierPrefix settings
    else addChar $ modifierSeparator settings
  case varMod of
    Normal -> do
      when (modifierSupportsNamed settings) (namePrefix settings varName val)
      unexploded
    Explode -> exploded
    (MaxLength l) -> do
      when (modifierSupportsNamed settings) (namePrefix settings varName val)
      censor (fromList . take l . toList) unexploded
  where
    settings = options m
    sepByCommas = sequence_ . intersperse (addChar ',')
    associativeCommas (n, v) = addString n >> addChar ',' >> addSingle v
    unexploded = case val of
      (Associative l) -> sepByCommas $ map associativeCommas l
      (List l) -> sepByCommas $ map addSingle l
      s@(Single _) -> addSingle s
    explodedAssociative (k, v) = do
      addString k
      addChar '='
      addSingle v
    exploded :: StringBuilder ()
    exploded = case val of
      (Single s) -> do
        when (modifierSupportsNamed settings) (namePrefix settings varName val)
        addString s
      (Associative l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map explodedAssociative l
      (List l) -> sequence_ $ intersperse (addChar $ modifierSeparator settings) $ map addSingle l

processVariables :: forall a. [(String, TemplateValue a)] -> Modifier -> [Variable] -> StringBuilder ()
processVariables env m vs = sequence_ $ intersperse (addChar $ modifierSeparator $ options m) $ processedVariables
  where
    findValue (Variable varName _) = lookup varName env
    nonEmptyVariables :: [(Variable, TemplateValue a)]
    nonEmptyVariables = catMaybes $ map (\v -> fmap (\mv -> (v, mv)) $ findValue v) vs
    processors :: forall a. [Variable -> TemplateValue a -> StringBuilder ()]
    processors = (processVariable m True) : repeat (processVariable m False)
    processedVariables :: [StringBuilder ()]
    processedVariables = zipWith uncurry processors nonEmptyVariables

render :: forall a. UriTemplate -> [(String, TemplateValue a)] -> String
render tpl env = toList $ execWriter $ mapM_ go tpl
  where
    go :: TemplateSegment -> StringBuilder ()
    go (Literal s) = addString s
    go (Embed m vs) = processVariables env m vs
      {-(processVariable m True) : repeat (processVariable m False)-}
