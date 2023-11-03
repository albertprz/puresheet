module App.SyntaxTree.FnDef where

import FatPrelude
import Prim hiding (Type)

import App.Components.Table.Cell (Cell, CellValue)
import App.SyntaxTree.Common (Module, QVar, QVarOp, Var, VarOp)
import App.SyntaxTree.Pattern (Pattern)
import App.SyntaxTree.Type (Type)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafeCrashWith)

data OpDef = OpDef VarOp Var Associativity Precedence

data FnDef = FnDef Var (Array (Var /\ Maybe Type)) (Maybe Type) FnBody

data FnBody
  = FnApply FnBody (Array FnBody)
  | LambdaFn (Array Var) FnBody
  | InfixFnApply (Array QVarOp) (Array FnBody)
  | LeftOpSection QVarOp FnBody
  | RightOpSection FnBody QVarOp
  | WhereExpr FnBody (Array FnDef)
  | CondExpr (Array GuardedFnBody)
  | SwitchExpr FnBody (Array CaseBinding)
  | CellMatrixRange Cell Cell
  | CellArrayRange Cell Cell
  | ArrayRange FnBody FnBody
  | Array' (Array FnBody)
  | FnVar QVar
  | FnOp QVarOp
  | Cell' Cell
  | CellValue' CellValue
  | Object' Object

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

newtype FnInfo =
  FnInfo
    { id :: Maybe FnId
    , body :: FnBody
    , params :: Array Var
    , scope :: Scope
    , argsMap :: Map (Scope /\ Var) FnInfo
    }

type FnId = { fnModule :: Module, fnName :: Var }

type BuiltinFnInfo =
  { fn :: Array Object -> Object
  , arity :: Arity
  , defaultParams :: Set Int
  }

type OpInfo =
  { id :: FnOpId
  , fnName :: QVar
  , precedence :: Precedence
  , associativity :: Associativity
  }

type FnOpId = { opModule :: Module, opName :: VarOp }

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
  = P0
  | P1
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

instance Show Object where
  show = case _ of
    (BoolObj x) -> show x
    (IntObj x) -> show x
    (FloatObj x) -> show x
    (CharObj x) -> show x
    (StringObj x) -> show x
    (ArrayObj x) -> show x
    (FnObj _) -> "function"
    (BuiltinFnObj _) -> "builtin-function"
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

derive newtype instance Eq Scope
derive newtype instance Ord Scope
derive newtype instance Semiring Scope
derive newtype instance Show Scope

instance Range Scope where
  range (Scope a) (Scope b) = Scope <$> (a .. b)

derive instance Eq Associativity
derive instance Eq Precedence
derive instance Ord Precedence
derive instance Generic Precedence _

instance Enum Precedence where
  succ = genericSucc
  pred = genericPred

instance Bounded Precedence where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum Precedence where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

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

derive instance Generic FnInfo _
instance Show FnInfo where
  show x = genericShow x

derive instance Generic FnDef _
instance Show FnDef where
  show x = genericShow x

derive instance Generic FnBody _
instance Show FnBody where
  show x = genericShow x

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
