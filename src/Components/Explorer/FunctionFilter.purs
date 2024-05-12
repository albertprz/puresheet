module App.Components.Explorer.FunctionFilter where

import FatPrelude
import Prim hiding (Type)

import App.Editor.Suggestion (SuggestionInfo(..), SuggestionTerm(..), partialQVar, partialQVarOp)
import App.Evaluator.Common (LocalFormulaCtx)
import App.Evaluator.Formula (evalExprInCtx)
import App.Parser.Common (token)
import App.Parser.FnDef (fnBody)
import App.Parser.Type (type')
import App.SyntaxTree.Common (Module, QVar(..), QVarOp)
import App.SyntaxTree.FnDef (FnBody(..), FnSig, Object)
import App.SyntaxTree.Type (Type(..), TypeParam)
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (is, someSepBy)
import Bookhound.Parsers.Char (comma)
import Bookhound.Parsers.String (betweenParens)
import Data.Array as Array
import Data.Map as Map
import Data.String.Utils as String

data FnFilter
  = FnName QVar
  | OpName QVarOp
  | FnExample (Array FnBody) FnBody
  | FnSignature (Array Type) Type

derive instance Eq FnFilter

instance Show FnFilter where
  show = case _ of
    FnName x -> show x
    OpName x -> show x
    _ -> mempty

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
    any (_ `matchesType` targetType) sourceType
    where
    sourceType = buildFnSigType info.fnSig
    targetType = buildFunctionType paramTypes returnType

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

buildFnSigType :: FnSig -> Maybe Type
buildFnSigType { params, returnType } =
  buildFunctionType <$> traverse snd params <*> returnType

buildFunctionType :: Array Type -> Type -> Type
buildFunctionType paramTypes returnType =
  ArrowTypeApply $ Array.snoc paramTypes returnType

countFnSigParams :: FnSig -> Int
countFnSigParams fnSig =
  maybe zero countParams $ buildFnSigType fnSig

countParams :: Type -> Int
countParams = case _ of
  TypeApply x xs -> sum $ map countParams (Array.cons x xs)
  ArrowTypeApply xs -> sum $ map countParams xs
  ArrayTypeApply x -> countParams x
  TypeParam' _ -> one
  _ -> zero

matchesType :: Type -> Type -> Boolean
matchesType sourceType targetType =
  replacedType == targetType
  where
  replacedType = replaceParams replacements sourceType
  replacements = findParamReplacements (sourceType /\ targetType)

findParamReplacements :: (Type /\ Type) -> Map TypeParam Type
findParamReplacements = case _ of
  TypeApply x xs /\ TypeApply y ys ->
    Map.unions $ map findParamReplacements
      $ Array.zip (Array.cons x xs) (Array.cons y ys)
  ArrowTypeApply xs /\ ArrowTypeApply ys ->
    Map.unions $ map findParamReplacements $ Array.zip xs ys
  ArrayTypeApply x /\ ArrayTypeApply y ->
    findParamReplacements (x /\ y)
  TypeParam' param /\ targetType ->
    Map.singleton param targetType
  _ /\ _ -> Map.empty

replaceParams :: Map TypeParam Type -> Type -> Type
replaceParams replacements = case _ of
  TypeApply x xs -> TypeApply (replace x) (map replace xs)
  ArrowTypeApply xs -> ArrowTypeApply $ map replace xs
  ArrayTypeApply x -> ArrayTypeApply $ replace x
  TypeParam' param
    | Just targetType <- Map.lookup param replacements -> targetType
    | otherwise -> TypeParam' param
  x -> x
  where
  replace x = replaceParams replacements x
