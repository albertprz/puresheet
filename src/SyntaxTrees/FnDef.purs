module App.SyntaxTrees.FnDef where

import FatPrelude

import App.Components.Table.Cell (Cell)
import App.SyntaxTrees.Common (Ctor, Literal, Var, VarOp)
import App.SyntaxTrees.Pattern (Pattern)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data FnDef = FnDef (Array Var) (Array Pattern) MaybeGuardedFnBody

data FnBody
  = FnApply FnBody (Array FnBody)
  | InfixFnApply (Array VarOp) (Array FnBody)
  | LeftOpSection VarOp FnBody
  | RightOpSection FnBody VarOp
  | Bindings FnBody (Array FnDef)
  | MultiWayIfExpr (Array GuardedFnBody)
  | SwitchExpr FnBody (Array CaseBinding)
  | RecordCreate Ctor (Array (Var /\ FnBody))
  | RecordUpdate FnBody (Array (Var /\ FnBody))
  | ListRange FnBody FnBody
  | List (Array FnBody)
  | FnVar' FnVar
  | FnOp VarOp
  | Literal' Literal
  | Cell' Cell

data FnVar
  = Selection Var (Array Var)
  | Var' Var
  | Ctor' Ctor

data CaseBinding = CaseBinding Pattern MaybeGuardedFnBody

data MaybeGuardedFnBody
  = Guarded (Array GuardedFnBody)
  | Standard FnBody

data GuardedFnBody = GuardedFnBody Guard FnBody

data Guard
  = Guard (Array PatternGuard)
  | Otherwise

data PatternGuard
  = PatternGuard Pattern FnBody
  | SimpleGuard FnBody

derive instance Generic FnDef _
instance Show FnDef where
  show x = genericShow x

derive instance Generic FnBody _
instance Show FnBody where
  show x = genericShow x

instance Show FnVar where
  show (Selection var fields) =
    show var <> intercalate "." (show <$> fields)

  show (Var' var) = show var
  show (Ctor' var) = show var

derive instance Generic CaseBinding _
instance Show CaseBinding where
  show x = genericShow x

derive instance Generic MaybeGuardedFnBody _
instance Show MaybeGuardedFnBody where
  show x = genericShow x

derive instance Generic GuardedFnBody _
instance Show GuardedFnBody where
  show x = genericShow x

derive instance Generic Guard _
instance Show Guard where
  show x = genericShow x

derive instance Generic PatternGuard _
instance Show PatternGuard where
  show x = genericShow x
