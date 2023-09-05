module App.Parsers.Common where

import FatPrelude

import App.Components.Table.Cell (CellValue(..))
import App.SyntaxTrees.Common (Ctor(..), Module(..), Var(..), VarOp(..))
import Bookhound.Parser (Parser, check, withTransform)
import Bookhound.ParserCombinators (inverse, is, maybeWithin, noneOf, oneOf, someSepBy, within, (->>-), (<|>), (|*), (|+), (|?), (||*))
import Bookhound.Parsers.Char (alpha, alphaNum, char, colon, comma, dot, lower, newLine, quote, underscore, upper)
import Bookhound.Parsers.Number (double, int)
import Bookhound.Parsers.String (spacing, withinDoubleQuotes, withinParens, withinQuotes)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.String.CodeUnits (singleton) as String
import Data.String.Unsafe (char) as String

cellValue :: Parser CellValue
cellValue = token
  $ (BoolVal <$> (true <$ is "true" <|> false <$ is "false"))
  <|> (IntVal <$> int)
  <|> (FloatVal <$> double)
  <|> (CharVal <$> withinQuotes (charLit <|> charLitEscaped))
  <|>
    ( StringVal <$> withinDoubleQuotes
        ((||*) (stringLit <|> charLitEscaped))
    )
  where
  charLit = noneOf [ '\'', '\\' ]
  charLitEscaped = String.char <<< wrapQuotes <$> (is '\\' ->>- alpha) <|>
    (is '\\' *> char)
  stringLit = noneOf [ '"', '\\' ]

var :: Parser Var
var = Var <$> notReserved
  ( withinParens (operator opSymbol <|> simpleOperator <|> simpleOperatorFn) <|>
      ident lower
  )

ctor :: Parser Ctor
ctor = Ctor <$> notReserved (withinParens (operator colon) <|> ident upper)

varOp :: Parser VarOp
varOp = VarOp <$> notReserved
  ( wrapBackQuotes <$> withinBackQuotes (ident lower) <|>
      (operator opSymbol <|> simpleOperator)
  )

argListOf :: forall a. Parser a -> Parser (Array a)
argListOf = withinParens <<< someSepBy comma

module' :: Parser Module
module' = Module <$> someSepBy dot (ident upper)

module'' :: Parser Module
module'' = Module <$> someSepBy dot (nonTokenIdent upper)

ident :: Parser Char -> Parser String
ident start = token $ start ->>- ((|*) idChar)

operator :: Parser Char -> Parser String
operator start = token $ start ->>- ((|*) (opSymbol <|> colon))

simpleOperator :: Parser String
simpleOperator = token $ oneOf [ ":" ]

simpleOperatorFn :: Parser String
simpleOperatorFn = token $ oneOf [ ",", ",,", ",,," ]

nonTokenIdent :: Parser Char -> Parser String
nonTokenIdent start = start ->>- (|*) idChar

idChar :: Parser Char
idChar = alphaNum <|> underscore <|> quote

opSymbol :: Parser Char
opSymbol = oneOf symbolChars

token :: forall a. Parser a -> Parser a
token =
  withTransform (maybeWithin ((|+) lineComment) <<< maybeWithin spacing)

qTerm :: forall a. Parser a -> Parser (Maybe Module /\ a)
qTerm x = (/\) <$> (|?) (module'' <* dot) <*> x

qTerm' :: forall b. (String -> b) -> Parser (Maybe Module /\ b)
qTerm' fn = token parser
  where
  parser = do
    xs <- getComponents <$> module''
    pure $
      ( (Module <$> (foldMap wrapMaybe $ init' xs)) /\
          (fn $ unsafeFromJust $ last' xs)
      )
  getComponents (Module xs) = xs

lineComment :: Parser String
lineComment = is "--" ->>-
  ( ( String.singleton <$> newLine <|> noneOf symbolChars ->>-
        (|*) (inverse newLine)
    )
  )

notReserved :: Parser String -> Parser String
notReserved = check "reserved"
  (_ `notElem` (reservedSymbols <> reservedKeyWords))

withinBackQuotes :: forall b. Parser b -> Parser b
withinBackQuotes = within (is '`')

symbolChars :: Array Char
symbolChars =
  [ '!'
  , '#'
  , '$'
  , '%'
  , '&'
  , '*'
  , '+'
  , '.'
  , '/'
  , '<'
  , '='
  , '>'
  , '?'
  , '@'
  , '\\'
  , '|'
  , '^'
  , '|'
  , '-'
  , '~'
  ]

reservedKeyWords :: Array String
reservedKeyWords =
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "forall"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "qualified"
  , "then"
  , "type"
  , "where"
  , "_"
  , "as"
  ]

reservedSymbols :: Array String
reservedSymbols =
  [ "..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>", "[", "]" ]
