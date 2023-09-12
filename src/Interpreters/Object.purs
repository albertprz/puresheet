module App.Interpreters.Object where

import FatPrelude

import App.Components.Table.Cell (CellValue(..))
import App.SyntaxTrees.FnDef (Object(..))
import Matrix (Matrix)
import Matrix as Matrix

cellValueToObj :: CellValue -> Object
cellValueToObj = case _ of
  BoolVal x -> BoolObj x
  IntVal x -> IntObj x
  FloatVal x -> FloatObj x
  CharVal x -> CharObj x
  StringVal x -> StringObj x

objectToCellValues :: Partial => Object -> Maybe (Matrix CellValue)
objectToCellValues = Matrix.fromArray <<< cellValues
  where
  cellValues = case _ of
    ListObj xs
      | Just xss <- traverse extractShallowList xs ->
          objectToCellValue <$$> xss
    ListObj xs
      | all isElement xs ->
          [ objectToCellValue <$> xs ]
    x -> [ [ objectToCellValue x ] ]

objectToCellValue :: Partial => Object -> CellValue
objectToCellValue = case _ of
  BoolObj x -> BoolVal x
  IntObj x -> IntVal x
  FloatObj x -> FloatVal x
  CharObj x -> CharVal x
  StringObj x -> StringVal x
  NullObj -> StringVal ""

extractBool :: Object -> Either Error Boolean
extractBool (BoolObj x) = pure x
extractBool _ = Left $ error "guard expression does not return Bool"

extractShallowList :: Object -> Maybe (Array Object)
extractShallowList (ListObj xs) | all isElement xs = Just xs
extractShallowList _ = Nothing

isElement :: Object -> Boolean
isElement (ListObj _) = false
isElement _ = true

extractList :: Object -> Maybe (Array Object)
extractList (ListObj xs) = Just xs
extractList _ = Nothing

extractNList :: Int -> Object -> Maybe (Array Object)
extractNList n (ListObj xs) | length xs == n = Just xs
extractNList _ _ = Nothing

nonNullObj :: Object -> Boolean
nonNullObj NullObj = false
nonNullObj _ = true
