module App.Components.Explorer.FunctionFilter where

import FatPrelude
import Prim hiding (Type)

import App.Editor.Suggestion (SuggestionTerm(..), partialQVar, partialQVarOp)
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Formula (evalExprInCtx)
import App.Explorer.Suggestion (SuggestionInfo(..))
import App.Parser.Common (token)
import App.Parser.FnDef (fnBody)
import App.Parser.Type (type')
import App.SyntaxTree.Common (Module, QVar(..), QVarOp)
import App.SyntaxTree.FnDef (FnBody(..), Object)
import App.SyntaxTree.Type (Type)
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (is, someSepBy)
import Bookhound.Parsers.Char (comma)
import Bookhound.Parsers.String (betweenParens)
import Data.String.Utils as String

data FnFilter
  = FnName QVar
  | OpName QVarOp
  | FnExample (Array FnBody) FnBody
  | FnSignature (Array Type) Type

derive instance Eq FnFilter

parseFnFilter :: String -> Maybe FnFilter
parseFnFilter str = hush $ runParser parser str
  where
  parser = map FnName partialQVar
    <|> map OpName partialQVarOp
    <|> exampleParser
    <|> signatureParser

  exampleParser = FnExample
    <$> betweenParens (token $ someSepBy comma fnBody)
    <*> (is "=" *> fnBody)

  signatureParser = FnSignature
    <$> betweenParens (token $ someSepBy comma type')
    <*> (is ":" *> type')

termPredicate
  :: LocalFormulaCtx
  -> Maybe FnFilter
  -> Maybe Module
  -> SuggestionInfo
  -> Boolean
termPredicate ctx fnFilter module' (SuggestionInfo info) =
  case fnFilter /\ info.term of
    Nothing /\ _ -> true
    Just (FnName x) /\ FnSuggestion y -> isNameLike x y
    Just (FnName x) /\ BuiltinFnSuggestion y -> isNameLike x y
    Just (OpName x) /\ OpSuggestion y -> isNameLike x y
    Just (FnExample x y) /\ _ -> isExampleOf x y
    Just (FnSignature x y) /\ _ -> isSignatureOf x y
    Just _ /\ _ -> false

  where
  isExampleOf args result =
    evalExample info.fn args ctx == evalResult info.fn result ctx

  isSignatureOf paramTypes returnType =
    filterMap snd info.fnSig.params == paramTypes
      && (info.fnSig.returnType == Just returnType)

  isNameLike :: forall a. Show a => a -> a -> Boolean
  isNameLike x y =
    String.startsWith (show x) (show y)
      || String.startsWith (foldMap show module' <> "." <> show x) (show y)

evalExample :: QVar -> Array FnBody -> LocalFormulaCtx -> Maybe Object
evalExample qVar@(QVar module' _) args ctx =
  hush $ evalExprInCtx ctx' expr
  where
  ctx' = ctx { module' = unsafeFromJust module' }
  expr = FnApply (FnVar qVar) args

evalResult :: QVar -> FnBody -> LocalFormulaCtx -> Maybe Object
evalResult (QVar module' _) result ctx =
  hush $ evalExprInCtx ctx' result
  where
  ctx' = ctx { module' = unsafeFromJust module' }
