module App.Components.Table.SyntaxAtom where

import FatPrelude

import App.CSS.ClassNames (cellSyntax, keywordSyntax, numberSyntax, operatorSyntax, regularSyntax, stringSyntax, symbolSyntax)
import App.Components.Table.Cell (cellParser, double, int, showCell)
import App.Parser.Common (notReserved, opSymbol, qTerm, reservedKeyWords, reservedSymbols)
import App.SyntaxTree.Common (QVarOp(..), Var(..), VarOp(..))
import App.Utils.String (wrapDoubleQuotes)
import Bookhound.Parser (Parser, anyOf)
import Bookhound.ParserCombinators (is, noneOf, (->>-), (<|>), (|*), (|+), (||*))
import Bookhound.Parsers.Char (alpha, alphaNum, char, colon, lower, quote, underscore)
import Bookhound.Parsers.String (withinDoubleQuotes, withinQuotes)
import Data.String.CodeUnits (singleton) as String
import Data.String.Unsafe (char) as String
import Web.HTML.Common (ClassName)

syntaxAtomToClassName :: SyntaxAtom -> ClassName
syntaxAtomToClassName = case _ of
  Cell' _ -> cellSyntax
  Number' _ -> numberSyntax
  String' _ -> stringSyntax
  Char' _ -> stringSyntax
  Keyword _ -> keywordSyntax
  Symbol _ -> symbolSyntax
  Operator _ -> operatorSyntax
  OtherText _ -> regularSyntax

condenseSyntaxAtoms :: Array SyntaxAtom -> Array SyntaxAtom
condenseSyntaxAtoms = foldl go []
  where
  go xs (OtherText y)
    | Just { init, last: (OtherText x) } <- unsnoc' xs =
        toArray $ snoc' init $ OtherText (x <> y)
  go xs y = toArray $ snoc' xs y

syntaxAtom :: Parser (Array SyntaxAtom)
syntaxAtom = (|+) atom
  where
  atom =
    Number' <$> (show <$> double <|> show <$> int)
      <|> Char'
      <<< wrapQuotes
      <<< String.singleton
      <$> withinQuotes (charLit <|> charLitEscaped)
      <|>
        String'
      <<< wrapDoubleQuotes
      <$> withinDoubleQuotes
        ((||*) (stringLit <|> charLitEscaped))
      <|> Cell'
      <<< showCell
      <$> cellParser
      <|> Keyword
      <$> (anyOf (is <$> reservedKeyWords))
      <|> Symbol
      <$> (anyOf (is <$> reservedSymbols))
      <|> Operator
      <$> qVarOp
      <|> OtherText
      <<< String.singleton
      <$> char

  charLit = noneOf [ '\'', '\\' ]
  charLitEscaped = String.char <<< wrapQuotes <$> (is '\\' ->>- alpha) <|>
    (is '\\' *> char)
  stringLit = noneOf [ '"', '\\' ]
  operator start = start ->>- ((|*) (opSymbol <|> colon))
  idChar = alphaNum <|> underscore <|> quote
  ident start = start ->>- ((|*) idChar)
  var = Var <$> notReserved (ident lower)
  varOp = VarOp <$> notReserved (operator opSymbol)
  qVarOp = uncurry QVarOp <$> qTerm
    ( VarOp <$> (is "|" ->>- (extractVar <$> var) ->>- is ">")
        <|> VarOp
        <$> (is "<" ->>- (extractVar <$> var) ->>- is "|")
        <|> varOp
    )
  extractVar (Var x) = x

data SyntaxAtom
  = Cell' String
  | Number' String
  | String' String
  | Char' String
  | Keyword String
  | Symbol String
  | Operator QVarOp
  | OtherText String

instance Show SyntaxAtom where
  show (Cell' x) = x
  show (Number' x) = x
  show (String' x) = x
  show (Char' x) = x
  show (Keyword x) = x
  show (Symbol x) = x
  show (Operator x) = show x
  show (OtherText x) = x
