module App.Evaluator.Errors where

import FatPrelude

import App.Components.Table.Cell (Cell, showCell)
import App.SyntaxTree.Common (QVar, QVarOp)
import App.SyntaxTree.FnDef (Object)

data EvalError
  = TypeError' TypeError
  | MatchError' MatchError
  | LexicalError' LexicalError
  | SerializationError' SerializationError

data LexicalError
  = UnknownValue QVar
  | UnknownOperator QVarOp

data MatchError
  = NonExhaustiveMatch
  | NonExhaustiveGuard
  | InvalidGuard

data TypeError
  = TooManyArguments Int
  | NotAFunction Object
  | InvalidArgumentTypes (Array Object)
  | InvalidCellArrayRange Cell Cell

data SerializationError = CellValueSerializationError

derive instance Eq EvalError
derive instance Eq TypeError
derive instance Eq MatchError
derive instance Eq LexicalError
derive instance Eq SerializationError

instance Show EvalError where
  show (LexicalError' x) = "Lexical Error: " <> show x
  show (MatchError' x) = "Match Error: " <> show x
  show (TypeError' x) = "Type Error: " <> show x
  show (SerializationError' x) = "Serialization Error: " <> show x

instance Semigroup EvalError where
  append x _ = x

instance Show LexicalError where
  show (UnknownValue x) = "Unknown value " <> wrapQuotes (show x)
  show (UnknownOperator x) = "Unknown operator " <> wrapQuotes (show x)

instance Show MatchError where
  show NonExhaustiveMatch = "Failed pattern match (non exhaustive)"
  show NonExhaustiveGuard = "Failed guard match (non exhaustive)"
  show InvalidGuard = "Guard expression did not return a Boolean value"

instance Show TypeError where
  show (TooManyArguments x) = "Too many arguments applied: " <> show x
  show (NotAFunction x) = "Value " <> wrapQuotes (show x) <>
    " is not a function"
  show (InvalidArgumentTypes x) =
    "The combination of argument types is not allowed: " <> showParensCsv x
  show (InvalidCellArrayRange x y) =
    "Cell array ranges only work for cells in the same row / column: "
      <> showCell x
      <> " "
      <> showCell y

instance Show SerializationError where
  show CellValueSerializationError =
    "Could not serialize evaluation result to a non-empty matrix of cell values"

raiseError :: forall m a. Monad m => EvalError -> ExceptT EvalError m a
raiseError x = except $ Left x
