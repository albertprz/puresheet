module App.SyntaxTree.FnDef where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue)
import App.SyntaxTree.Common (Var, VarOp)
import App.SyntaxTree.Pattern (Pattern)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafeCrashWith)

data FnDef = FnDef Var (Array Var) FnBody

data FnBody
  = FnApply FnBody (Array FnBody)
  | InfixFnApply (Array VarOp) (Array FnBody)
  | LeftOpSection VarOp FnBody
  | RightOpSection FnBody VarOp
  | WhereExpr FnBody (Array FnDef)
  | CondExpr (Array GuardedFnBody)
  | SwitchExpr FnBody (Array CaseBinding)
  | ListRange FnBody FnBody
  | MatrixRange Cell Cell
  | List (Array FnBody)
  | FnVar' FnVar
  | FnOp VarOp
  | Cell' Cell
  | CellValue' CellValue
  | Object' Object

data FnVar = Var' Var

-- | Ctor' Ctor

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
  | ArrayObj (Array Object)
  | FnObj FnInfo
  | BuiltinFnObj BuiltinFnInfo
  | NullObj

type FnInfo =
  { body :: FnBody
  , params :: Array Var
  , scope :: Scope
  }

type BuiltinFnInfo =
  { fn :: Array Object -> Object
  , arity :: Arity
  , defaultParams :: Set Int
  }

type OpInfo =
  { fnName :: Var
  , precedence :: Precedence
  , associativity :: Associativity
  }

data Arity
  = A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | A8
  | A9

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

data Associativity
  = L
  | R

newtype Scope = Scope Int

derive newtype instance Eq Scope
derive newtype instance Ord Scope
derive newtype instance Semiring Scope
derive newtype instance Show Scope

instance Range Scope where
  range (Scope a) (Scope b) = Scope <$> (a .. b)

derive instance Eq Precedence
derive instance Ord Precedence

derive instance Eq Arity
derive instance Ord Arity
derive instance Generic Arity _

instance Enum Arity where
  succ = genericSucc
  pred = genericPred

instance Bounded Arity where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum Arity where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

derive instance Generic FnDef _
instance Show FnDef where
  show x = genericShow x

derive instance Generic FnBody _
instance Show FnBody where
  show x = genericShow x

instance Show FnVar where
  show (Var' var) = show var

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

instance Show Object where
  show = case _ of
    (BoolObj x) -> show x
    (IntObj x) -> show x
    (FloatObj x) -> show x
    (CharObj x) -> show x
    (StringObj x) -> show x
    (ArrayObj x) -> show x
    (FnObj _) -> "[fn]"
    (BuiltinFnObj _) -> "[builtin fn]"
    (NullObj) -> "null"

instance Eq Object where
  eq (BoolObj x) (BoolObj y) = x == y
  eq (IntObj x) (IntObj y) = x == y
  eq (FloatObj x) (FloatObj y) = x == y
  eq (CharObj x) (CharObj y) = x == y
  eq (StringObj x) (StringObj y) = x == y
  eq (ArrayObj x) (ArrayObj y) = x == y
  eq NullObj NullObj = true
  eq NullObj _ = false
  eq _ NullObj = false
  eq _ _ = false

instance Ord Object where
  compare (BoolObj x) (BoolObj y) = compare x y
  compare (IntObj x) (IntObj y) = compare x y
  compare (FloatObj x) (FloatObj y) = compare x y
  compare (CharObj x) (CharObj y) = compare x y
  compare (StringObj x) (StringObj y) = compare x y
  compare (ArrayObj x) (ArrayObj y) = compare x y
  compare NullObj NullObj = EQ
  compare x y = unsafeCrashWith
    ("Cannot compare: " <> show x <> " and " <> show y)
