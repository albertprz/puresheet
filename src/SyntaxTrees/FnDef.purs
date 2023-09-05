module App.SyntaxTrees.FnDef where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.SyntaxTrees.Common (Ctor, Var, VarOp)
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
  | CellValue' CellValue
  | Cell' Cell
  | Object' Object

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

data Object
  = BoolObj Boolean
  | IntObj Int
  | FloatObj Number
  | CharObj Char
  | StringObj String
  | ListObj (Array Object)
  | FnObj FnInfo

type FnInfo =
  { body :: FnBody
  , params :: Array Var
  }

type OpInfo =
  { fnName :: Var
  , precedence :: Precedence
  , associativity :: Associativity
  }

data Precedence
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  | P7
  | P8
  | P9
  | P10
  | P11
  | P12

derive instance Eq Precedence
derive instance Ord Precedence

data Associativity
  = L
  | R

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

derive instance Generic Object _
instance Show Object where
  show x = genericShow x
