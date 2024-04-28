module App.Evaluator.Object where

import FatPrelude

import App.Components.Spreadsheet.Cell (CellValue(..))
import App.Evaluator.Errors (EvalError(..), MatchError(..))
import App.SyntaxTree.FnDef (Object(..))
import Data.Array as Array
import Data.List as List
import Data.String (codePointFromChar)
import Data.String.CodePoints (singleton) as String
import Data.String.Unsafe (char) as String
import Matrix (Matrix)
import Matrix as Matrix

cellValueToObj :: CellValue -> Object
cellValueToObj = case _ of
  BoolVal x -> BoolObj x
  IntVal x -> IntObj x
  FloatVal x -> FloatObj x
  CharVal x -> CharObj $ String.char $ String.singleton x
  StringVal x -> StringObj x

objectToCellValues :: Partial => Object -> Maybe (Matrix CellValue)
objectToCellValues = Matrix.fromArray <<< cellValues
  where
  cellValues = case _ of
    ListObj xs -> cellValues $ ArrayObj $ Array.fromFoldable xs
    ArrayObj xs
      | Just xss <- traverse extractShallowList xs ->
          objectToCellValue <$$> xss
    ArrayObj xs
      | all isElement xs ->
          [ objectToCellValue <$> xs ]
    x -> [ [ objectToCellValue x ] ]

objectToCellValue :: Partial => Object -> CellValue
objectToCellValue = case _ of
  BoolObj x -> BoolVal x
  IntObj x -> IntVal x
  FloatObj x -> FloatVal x
  CharObj x -> CharVal $ codePointFromChar x
  StringObj x -> StringVal x
  NullObj -> StringVal ""

extractBool :: Object -> Either EvalError Boolean
extractBool (BoolObj x) = pure x
extractBool _ = Left $ MatchError' InvalidGuard

extractShallowList :: Object -> Maybe (Array Object)
extractShallowList (ArrayObj xs) | all isElement xs = Just xs
extractShallowList (ListObj xs) | all isElement xs =
  Just $ Array.fromFoldable xs
extractShallowList _ = Nothing

isElement :: Object -> Boolean
isElement (ArrayObj _) = false
isElement (ListObj _) = false
isElement _ = true

extractString :: Object -> Maybe String
extractString (StringObj str) = Just str
extractString _ = Nothing

extractList :: Object -> Maybe (Array Object)
extractList (ArrayObj xs) = Just xs
extractList (ListObj xs) = Just $ Array.fromFoldable xs
extractList _ = Nothing

extractNList :: Int -> Object -> Maybe (Array Object)
extractNList n (ArrayObj xs) | Array.length xs == n = Just xs
extractNList n (ListObj xs) | List.length xs == n = Just
  $ Array.fromFoldable xs
extractNList _ _ = Nothing

nonNullObj :: Object -> Boolean
nonNullObj NullObj = false
nonNullObj _ = true
