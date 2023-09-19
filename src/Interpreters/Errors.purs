module App.Interpreters.Errors where

import FatPrelude

import App.SyntaxTrees.Common (Var, VarOp)
import App.SyntaxTrees.FnDef (Object)
import Control.Monad.Except (ExceptT, except)

data EvalError
  = TypeError' TypeError
  | MatchError' MatchError
  | LexicalError' LexicalError

data TypeError
  = TooManyArguments Int
  | NotAFunction Object
  | InvalidArgumentTypes (Array Object)

data MatchError
  = NonExhaustiveMatch
  | InvalidGuard

data LexicalError
  = UnknownValue Var
  | UnknownOperator VarOp

derive instance Eq EvalError

derive instance Eq TypeError
derive instance Eq MatchError
derive instance Eq LexicalError

instance Show EvalError where
  show (TypeError' x) = "Type Error: " <> show x
  show (MatchError' x) = "Match Error: " <> show x
  show (LexicalError' x) = "Lexical Error: " <> show x

instance Show TypeError where
  show (TooManyArguments x) = "Too many arguments applied: " <> show x
  show (NotAFunction x) = "Value " <> wrapQuotes (show x) <>
    " is not m function"
  show (InvalidArgumentTypes x) =
    "The combination of argument types is not allowed: " <> showParensCsv x

instance Show MatchError where
  show NonExhaustiveMatch = "Failed pattern match (non-exhaustive)"
  show InvalidGuard = "Guard expression did not return a Boolean value"

instance Show LexicalError where
  show (UnknownValue x) = "Unknown value " <> wrapQuotes (show x)
  show (UnknownOperator x) = "Unknown operator " <> wrapQuotes (show x)

raiseEmptyError :: forall m a. Monad m => ExceptT EvalError m a
raiseEmptyError = except <<< Left $ emptyError

raiseError :: forall m a. Monad m => EvalError -> ExceptT EvalError m a
raiseError x = except $ Left x

emptyError :: EvalError
emptyError = MatchError' NonExhaustiveMatch
