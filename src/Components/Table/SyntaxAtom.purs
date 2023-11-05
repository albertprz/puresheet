module App.Components.Table.SyntaxAtom where

import FatPrelude
import Prim hiding (Type)

import App.CSS.ClassNames (cellSyntax, keywordSyntax, numberSyntax, operatorSyntax, regularSyntax, stringSyntax, symbolSyntax)
import App.Components.Table.Cell (cellParser, double, int, showCell)
import App.Parser.Common (notReserved, opSymbol, reservedKeyWords, reservedSymbols)
import App.SyntaxTree.Common (QVar(..))
import App.SyntaxTree.FnDef (FnInfo(..))
import App.SyntaxTree.Type (Type(..))
import App.Utils.String (wrapDoubleQuotes)
import Bookhound.Parser (Parser, anyOf)
import Bookhound.ParserCombinators (is, noneOf, (->>-), (<|>), (|*), (|+), (||*))
import Bookhound.Parsers.Char (alpha, alphaNum, lower, quote, underscore)
import Bookhound.Parsers.Char as Parsers
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

syntaxAtomParser :: Parser (Array SyntaxAtom)
syntaxAtomParser = (|+) atom
  where
  atom =
    (Number' <$> (map show double <|> map show int))
      <|> (Char' <$> char)
      <|> (String' <$> string)
      <|> (Cell' <<< showCell <$> cellParser)
      <|> (Keyword <$> (anyOf $ map is reservedKeyWords))
      <|> (Symbol <$> (anyOf $ map is reservedSymbols))
      <|> (Operator <$> varOp)
      <|> (OtherText <<< String.singleton <$> Parsers.char)

  char = wrapQuotes <<< String.singleton
    <$> withinQuotes (charLit <|> charLitEscaped)
  string = wrapDoubleQuotes
    <$> withinDoubleQuotes ((||*) (stringLit <|> charLitEscaped))
  charLit = noneOf [ '\'', '\\' ]
  charLitEscaped = String.char <<< wrapQuotes <$> (is '\\' ->>- alpha)
    <|> (is '\\' *> Parsers.char)
  stringLit = noneOf [ '"', '\\' ]
  operator start = start ->>- ((|*) opSymbol)
  idChar = alphaNum <|> underscore <|> quote
  ident start = start ->>- ((|*) idChar)
  var = notReserved (ident lower)
  varOp = (is "|" ->>- var ->>- is ">")
    <|> (is "<" ->>- var ->>- is "|")
    <|> notReserved (operator opSymbol)

fnInfoToSyntaxAtoms :: FnInfo -> Array SyntaxAtom
fnInfoToSyntaxAtoms (FnInfo { id: fnId, params, returnType })
  | Just { fnModule, fnName } <- fnId =
      fnName' <> params' <> returnType'
      where
      fnName' = [ Keyword (show (QVar (Just fnModule) fnName) <> " ") ]
      params' = wrapArgList $ foldMap param params
      returnType' = foldMap typeToSyntaxAtoms returnType
      param (var /\ type') = [ Keyword $ show var, Symbol ": " ]
        <> foldMap typeToSyntaxAtoms type'
  | otherwise = mempty

typeToSyntaxAtoms :: Type -> Array SyntaxAtom
typeToSyntaxAtoms = case _ of
  CtorTypeApply x ys -> typeApply x ys
  ParamTypeApply x ys -> typeApply x ys
  ArrowTypeApply xs -> infixTypeApply "->" xs
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
    <> wrapArgList (foldMap typeToSyntaxAtoms ys)

  infixTypeApply op = intersperse' (Operator (" " <> op <> " "))
    <<< foldMap typeToSyntaxAtoms

wrapArgList :: Array SyntaxAtom -> Array SyntaxAtom
wrapArgList = wrapParens <<< intersperse' (OtherText ", ")
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
  | OtherText String

instance Show SyntaxAtom where
  show (Cell' x) = x
  show (Number' x) = x
  show (String' x) = x
  show (Char' x) = x
  show (Keyword x) = x
  show (Symbol x) = x
  show (Operator x) = x
  show (OtherText x) = x
