module App.Utils.SyntaxAtom where

import FatPrelude
import Prim hiding (Type)

import App.CSS.ClassNames (cellSyntax, functionSyntax, keywordSyntax, numberSyntax, operatorSyntax, regularSyntax, stringSyntax, symbolSyntax)
import App.Components.Spreadsheet.Cell (cellParser, showCell)
import App.Parser.Common (nonTokenIdent, nonTokenOperator, notReservedKeyword, notReservedSymbol, reservedKeywords, reservedSymbols)
import App.SyntaxTree.Common (QVar(..))
import App.SyntaxTree.FnDef (FnSig)
import App.SyntaxTree.Type (Type(..))
import App.Utils.String (wrapDoubleQuotes)
import Bookhound.Parser (Parser)
import Bookhound.ParserCombinators (is, noneOf, oneOf, (->>-), (|+), (||*))
import Bookhound.Parsers.Char (alpha, lower, upper)
import Bookhound.Parsers.Char as Parsers
import Bookhound.Parsers.Number (double, int)
import Bookhound.Parsers.String (betweenDoubleQuotes, betweenQuotes)
import Data.Array as Array
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
  Function _ -> functionSyntax
  OtherText _ -> regularSyntax

condenseSyntaxAtoms :: Array SyntaxAtom -> Array SyntaxAtom
condenseSyntaxAtoms = foldl go []
  where
  go xs (OtherText y)
    | Just { init, last: (OtherText x) } <- Array.unsnoc xs =
        Array.snoc init $ OtherText (x <> y)
  go xs y = Array.snoc xs y

syntaxAtomParser :: Parser (Array SyntaxAtom)
syntaxAtomParser = (|+) atom
  where
  atom =
    (Number' <$> (map show double <|> map show int))
      <|> (Char' <$> char)
      <|> (String' <$> string)
      <|> (Cell' <<< showCell <$> cellParser)
      <|> (OtherText <$> module')
      <|> (Function <$> var)
      <|> (Operator <$> varOp)
      <|> (Keyword <$> oneOf reservedKeywords)
      <|> (Symbol <$> oneOf reservedSymbols)
      <|> (OtherText <<< String.singleton <$> Parsers.anyChar)

  char = wrapQuotes <<< String.singleton
    <$> betweenQuotes (charLit <|> charLitEscaped)
  string = wrapDoubleQuotes
    <$> betweenDoubleQuotes ((||*) (stringLit <|> charLitEscaped))
  charLit = noneOf [ '\'', '\\' ]
  charLitEscaped = String.char <<< wrapQuotes <$> (is '\\' ->>- alpha)
    <|> (is '\\' *> Parsers.anyChar)
  stringLit = noneOf [ '"', '\\' ]
  var = notReservedKeyword (nonTokenIdent lower)
  varOp = notReservedSymbol nonTokenOperator
  module' = nonTokenIdent upper

fnSigToSyntaxAtoms :: QVar -> FnSig -> Array SyntaxAtom
fnSigToSyntaxAtoms (QVar _ fnName) { params, returnType } =
  fnName' <> params' <> returnType'
  where
  fnName' = [ Function (show fnName <> " ") ]
  params' = wrapArgList (param <$> params)
  returnType' = foldMap annotation returnType
  annotation = ([ Symbol ": " ] <> _) <$> typeToSyntaxAtoms
  param (var /\ type') = [ Function $ show var ] <> foldMap annotation type'

typeToSyntaxAtoms :: Type -> Array SyntaxAtom
typeToSyntaxAtoms = case _ of
  VarTypeApply x ys -> typeApply x ys
  ParamTypeApply x ys -> typeApply x ys
  ArrowTypeApply xs -> infixTypeApply "➾" xs
  UnionTypeApply xs -> infixTypeApply "|" xs
  ArrayTypeApply x -> wrapSquare $ typeToSyntaxAtoms x
  TypeVar' x -> [ var x ]
  TypeParam' x -> [ var x ]

  where
  wrapSquare xs = [ OtherText "[" ] <> xs <> [ OtherText "]" ]

  var :: forall a. Show a => a -> SyntaxAtom
  var = Cell' <<< show

  typeApply :: forall a. Show a => a -> Array Type -> Array SyntaxAtom
  typeApply x ys = [ var x, OtherText " " ]
    <> wrapArgList (map typeToSyntaxAtoms ys)

  infixTypeApply op =
    intercalate [ Operator (" " <> op <> " ") ]
      <<< map typeToSyntaxAtoms

wrapArgList :: Array (Array SyntaxAtom) -> Array SyntaxAtom
wrapArgList = wrapParens <<< intercalate [ OtherText ", " ]
  where
  wrapParens xs = [ OtherText "(" ] <> xs <> [ OtherText ")" ]

data SyntaxAtom
  = Cell' String
  | Number' String
  | String' String
  | Char' String
  | Keyword String
  | Symbol String
  | Operator String
  | Function String
  | OtherText String

instance Show SyntaxAtom where
  show (Cell' x) = x
  show (Number' x) = x
  show (String' x) = x
  show (Char' x) = x
  show (Keyword x) = x
  show (Symbol x) = x
  show (Operator x) = x
  show (Function x) = x
  show (OtherText x) = x
