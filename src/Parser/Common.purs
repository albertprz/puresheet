module App.Parser.Common where

import FatPrelude

import App.Components.Table.Cell (CellValue(..))
import App.SyntaxTree.Common (Module(..), QVar(..), QVarOp(..), Var(..), VarOp(..))
import Bookhound.Parser (Parser, satisfy, withTransform)
import Bookhound.ParserCombinators (class IsMatch, is, maybeWithin, noneOf, oneOf, someSepBy, within, (->>-), (|*), (|?), (||*))
import Bookhound.Parsers.Char (alpha, alphaNum, anyChar, comma, dot, lower, upper)
import Bookhound.Parsers.Number (double, int)
import Bookhound.Parsers.String (spacing, withinDoubleQuotes, withinParens, withinQuotes)
import Data.String.Unsafe (char) as String

cellValue :: Parser CellValue
cellValue = token
  $ (FloatVal <$> double)
  <|> (IntVal <$> int)
  <|> (BoolVal <$> (true <$ isToken "true" <|> false <$ isToken "false"))
  <|> (CharVal <$> withinQuotes (charLit <|> charLitEscaped))
  <|>
    ( StringVal <$> withinDoubleQuotes
        ((||*) (stringLit <|> charLitEscaped))
    )
  where
  stringLit = noneOf [ '"', '\\' ]
  charLit = noneOf [ '\'', '\\' ]
  charLitEscaped = String.char <<< wrapQuotes <$> (is '\\' ->>- alpha)
    <|> (is '\\' *> anyChar)

var :: Parser Var
var = Var <$> notReserved (ident lower)

varOp :: Parser VarOp
varOp = VarOp <$> notReserved operator

module' :: Parser Module
module' = Module <$> someSepBy dot (ident upper)

qVar :: Parser QVar
qVar = uncurry QVar <$> qTerm var

qVarOp :: Parser QVarOp
qVarOp = uncurry QVarOp <$> qTerm
  ( VarOp <$> (isToken "|" ->>- map extractVar var ->>- isToken ">")
      <|> (VarOp <$> (isToken "<" ->>- map extractVar var ->>- isToken "|"))
      <|> varOp
  )
  where
  extractVar (Var x) = x

argListOf :: forall a. Parser a -> Parser (Array a)
argListOf = withinParens <<< someSepBy comma

ident :: Parser Char -> Parser String
ident start = token $ start ->>- (|*) alphaNum

operator :: Parser String
operator = token $ (||*) opSymbol

nonTokenIdent :: Parser Char -> Parser String
nonTokenIdent start = start ->>- (|*) alphaNum

idChar :: Parser Char
idChar = alphaNum

opSymbol :: Parser Char
opSymbol = oneOf opSymbolChars

token :: forall a. Parser a -> Parser a
token = withTransform (maybeWithin spacing)

isToken :: forall a. IsMatch a => a -> Parser a
isToken = token <<< is

qTerm :: forall a. Parser a -> Parser (Maybe Module /\ a)
qTerm x = (/\) <$> (|?) (moduleParser <* dot) <*> x
  where
  moduleParser = Module <$> someSepBy dot (nonTokenIdent upper)

notReserved :: Parser String -> Parser String
notReserved = satisfy
  $ flip notElem (reservedSymbols <> reservedKeyWords)

withinBackQuotes :: forall b. Parser b -> Parser b
withinBackQuotes = within $ isToken '`'

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

reservedKeyWords :: Array String
reservedKeyWords =
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
  , "["
  , "]"
  ]
