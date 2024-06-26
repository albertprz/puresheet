module App.Parser.Common where

import FatPrelude

import App.Components.Spreadsheet.Cell (CellValue(..))
import App.SyntaxTree.Common (Module(..), QVar(..), QVarOp(..), Var(..), VarOp(..))
import Bookhound.Parser (Parser, satisfy, withTransform)
import Bookhound.ParserCombinators (class IsMatch, is, noneOf, oneOf, someSepBy, surroundedBy, (->>-), (</\>), (|*), (|?), (||*), (||+))
import Bookhound.Parsers.Char (alpha, alphaNum, anyChar, comma, dot, lower, upper)
import Bookhound.Parsers.Number (double, int)
import Bookhound.Parsers.String (betweenDoubleQuotes, betweenParens, betweenQuotes, maybeBetweenSpacing)
import Data.String (codePointFromChar)
import Data.String.Unsafe (char) as String

cellValue :: Parser CellValue
cellValue = token
  $ (FloatVal <$> double)
  <|> (IntVal <$> int)
  <|>
    ( BoolVal <$>
        ( true <$ isToken "true"
            <|> (false <$ isToken "false")
        )
    )
  <|>
    ( CharVal <<< codePointFromChar <$> betweenQuotes
        (charLit <|> charLitEscaped)
    )
  <|>
    ( StringVal <$> betweenDoubleQuotes
        ((||*) (stringLit <|> charLitEscaped))
    )
  where
  stringLit = noneOf [ '\"', '\\' ]
  charLit = noneOf [ '\'', '\\' ]
  charLitEscaped = String.char <$> (is '\\' ->>- alpha)
    <|> (is '\\' *> anyChar)

var :: Parser Var
var = Var <$> notReservedKeyword (ident lower)

varOp :: Parser VarOp
varOp = VarOp <$> notReservedSymbol operator

module' :: Parser Module
module' = Module <$> someSepBy dot (ident upper)

qVar :: Parser QVar
qVar = uncurry QVar <$> qTerm var

qVarOp :: Parser QVarOp
qVarOp = uncurry QVarOp <$> qTerm
  ( VarOp <$> (is "|" ->>- map extractVar var ->>- is ">")
      <|> (VarOp <$> (is "<" ->>- map extractVar var ->>- is "|"))
      <|> varOp
  )
  where
  extractVar (Var x) = x

argListOf :: forall a. Parser a -> Parser (Array a)
argListOf = betweenParens <<< someSepBy comma

ident :: Parser Char -> Parser String
ident = token <<< nonTokenIdent

operator :: Parser String
operator = token nonTokenOperator

nonTokenIdent :: Parser Char -> Parser String
nonTokenIdent start = start ->>- (|*) alphaNum

nonTokenOperator :: Parser String
nonTokenOperator = (||+) opSymbol

idChar :: Parser Char
idChar = alphaNum

opSymbol :: Parser Char
opSymbol = oneOf opSymbolChars

token :: forall a. Parser a -> Parser a
token = withTransform maybeBetweenSpacing

isToken :: forall a. IsMatch a => a -> Parser a
isToken = token <<< is

qTerm :: forall a. Parser a -> Parser (Maybe Module /\ a)
qTerm x = (|?) (moduleParser <* dot) </\> x
  where
  moduleParser = Module <$> someSepBy dot (nonTokenIdent upper)

notReservedSymbol :: Parser String -> Parser String
notReservedSymbol = satisfy $ flip notElem reservedSymbols

notReservedKeyword :: Parser String -> Parser String
notReservedKeyword = satisfy $ flip notElem reservedKeywords

betweenBackQuotes :: forall b. Parser b -> Parser b
betweenBackQuotes = surroundedBy $ isToken '`'

opSymbolChars :: Array Char
opSymbolChars =
  [ '!'
  , '#'
  , '$'
  , '%'
  , '&'
  , '*'
  , '+'
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
  , ':'
  ]

reservedKeywords :: Array String
reservedKeywords =
  [ "module"
  , "import"
  , "as"
  , "op"
  , "def"
  , "where"
  , "switch"
  , "cond"
  , "_"
  , "otherwise"
  , "data"
  ]

reservedSymbols :: Array String
reservedSymbols =
  [ "..."
  , ".."
  , ":"
  , "<-"
  , "->"
  , "=>"
  , "="
  , "|"
  , "?"
  , "@"
  ]
