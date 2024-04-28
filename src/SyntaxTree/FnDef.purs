module App.SyntaxTree.FnDef where

import FatPrelude
import Prim hiding (Type)

import App.Components.Spreadsheet.Cell (Cell, CellValue)
import App.SyntaxTree.Common (Module, QVar, QVarOp, Var, VarOp)
import App.SyntaxTree.Pattern (Pattern)
import App.SyntaxTree.Type (Type)
import Data.Argonaut (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.HashMap as HashMap
import Data.Number.Format as Number
import Data.Show.Generic (genericShow)
import Data.String (CodePoint, codePointFromChar)
import Data.String.CodePoints (singleton) as String
import Data.String.Unsafe (char) as String
import Partial.Unsafe (unsafeCrashWith)
import Record (merge)

data OpDef = OpDef VarOp QVar Associativity Precedence

data FnDef = FnDef Var (Array (Var /\ Maybe Type)) (Maybe Type) String
  FnBody

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
  | ListObj (List Object)
  | ArrayObj (Array Object)
  | FnObj FnInfo
  | BuiltinFnObj BuiltinFnInfo
  | NullObj

newtype FnInfo = FnInfo
  { id :: Maybe FnId
  , body :: FnBody
  , scope :: Scope
  , argsMap :: HashMap (Scope /\ Var) FnInfo
  | FnSigRow
  }

type BuiltinFnInfo =
  { fn :: Array Object -> Object
  , defaultParams :: Set Int
  | FnSigRow
  }

type OpInfo =
  { id :: FnOpId
  , fnName :: QVar
  , precedence :: Precedence
  , associativity :: Associativity
  }

type FnId = { fnModule :: Module, fnName :: Var }

type FnOpId = { opModule :: Module, opName :: VarOp }

type FnSig = Record FnSigRow

type FnSigRow =
  ( params :: Array (Var /\ Maybe Type)
  , returnType :: Maybe Type
  , doc :: String
  )

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

data SerialObject
  = SerialBoolObj Boolean
  | SerialIntObj Int
  | SerialFloatObj Number
  | SerialCharObj CodePoint
  | SerialStringObj String
  | SerialListObj (List Object)
  | SerialArrayObj (Array Object)
  | SerialNullObj

instance Show Object where
  show = case _ of
    BoolObj x -> show x
    IntObj x -> show x
    FloatObj x -> Number.toStringWith (Number.fixed 3) x
    CharObj x -> show x
    StringObj x -> show x
    ListObj x -> show $ Array.fromFoldable x
    ArrayObj x -> show x
    FnObj _ -> "function"
    BuiltinFnObj _ -> "builtin-function"
    NullObj -> "null"

instance Eq Object where
  eq (BoolObj x) (BoolObj y) = x == y
  eq (IntObj x) (IntObj y) = x == y
  eq (FloatObj x) (FloatObj y) = x == y
  eq (CharObj x) (CharObj y) = x == y
  eq (StringObj x) (StringObj y) = x == y
  eq (ArrayObj x) (ArrayObj y) = x == y
  eq (ListObj x) (ListObj y) = x == y
  eq (ArrayObj x) (ListObj y) = x == Array.fromFoldable y
  eq (ListObj x) (ArrayObj y) = Array.fromFoldable x == y
  eq NullObj NullObj = true
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

derive instance Generic Object _
derive instance Generic SerialObject _

instance EncodeJson Object where
  encodeJson x = genericEncodeJson $ objectToSerialObject x

instance DecodeJson Object where
  decodeJson x = map serialObjectToObject $ genericDecodeJson x

instance EncodeJson SerialObject where
  encodeJson = genericEncodeJson

instance DecodeJson SerialObject where
  decodeJson = genericDecodeJson

serialObjectToObject :: SerialObject -> Object
serialObjectToObject = case _ of
  SerialBoolObj x -> BoolObj x
  SerialIntObj x -> IntObj x
  SerialFloatObj x -> FloatObj x
  SerialCharObj x -> CharObj $ String.char $ String.singleton x
  SerialStringObj x -> StringObj x
  SerialListObj x -> ListObj x
  SerialArrayObj x -> ArrayObj x
  SerialNullObj -> NullObj

objectToSerialObject :: Object -> SerialObject
objectToSerialObject = case _ of
  BoolObj x -> SerialBoolObj x
  IntObj x -> SerialIntObj x
  FloatObj x -> SerialFloatObj x
  CharObj x -> SerialCharObj $ codePointFromChar x
  StringObj x -> SerialStringObj x
  ListObj x -> SerialListObj x
  ArrayObj x -> SerialArrayObj x
  FnObj _ -> SerialNullObj
  BuiltinFnObj _ -> SerialNullObj
  NullObj -> SerialNullObj

derive newtype instance Eq Scope
derive newtype instance Ord Scope
derive newtype instance Semiring Scope
derive instance Newtype Scope _
derive newtype instance EncodeJson Scope
derive newtype instance DecodeJson Scope

instance Show Scope where
  show = show <<< unwrap

instance Hashable Scope where
  hash = unwrap

derive instance Eq Associativity
derive instance Generic Associativity _

instance EncodeJson Associativity where
  encodeJson = genericEncodeJson

instance DecodeJson Associativity where
  decodeJson = genericDecodeJson

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

instance EncodeJson Precedence where
  encodeJson = genericEncodeJson

instance DecodeJson Precedence where
  decodeJson = genericDecodeJson

derive instance Newtype FnInfo _

instance EncodeJson FnInfo where
  encodeJson (FnInfo fnInfo) = encodeJson $ merge
    { argsMap: HashMap.toArrayBy Tuple fnInfo.argsMap
    }
    fnInfo

instance DecodeJson FnInfo where
  decodeJson x = map fromParsed $ decodeJson x
    where
    fromParsed
      :: { id :: Maybe FnId
         , body :: FnBody
         , scope :: Scope
         , argsMap :: Array ((Scope /\ Var) /\ FnInfo)
         | FnSigRow
         }
      -> FnInfo
    fromParsed y = wrap $ merge
      { argsMap: HashMap.fromArray y.argsMap
      }
      y

derive instance Generic FnInfo _
instance Show FnInfo where
  show x = genericShow x

derive instance Eq FnInfo

derive instance Generic FnDef _
instance Show FnDef where
  show x = genericShow x

instance EncodeJson FnDef where
  encodeJson x = genericEncodeJson x

instance DecodeJson FnDef where
  decodeJson x = genericDecodeJson x

derive instance Eq FnDef

derive instance Generic FnBody _
instance Show FnBody where
  show x = genericShow x

instance EncodeJson FnBody where
  encodeJson x = genericEncodeJson x

instance DecodeJson FnBody where
  decodeJson x = genericDecodeJson x

derive instance Eq FnBody

derive instance Generic CaseBinding _
instance Show CaseBinding where
  show x = genericShow x

instance EncodeJson CaseBinding where
  encodeJson = genericEncodeJson

instance DecodeJson CaseBinding where
  decodeJson = genericDecodeJson

derive instance Eq CaseBinding

derive instance Generic MaybeGuardedFnBody _
instance Show MaybeGuardedFnBody where
  show x = genericShow x

instance EncodeJson MaybeGuardedFnBody where
  encodeJson = genericEncodeJson

instance DecodeJson MaybeGuardedFnBody where
  decodeJson = genericDecodeJson

derive instance Eq MaybeGuardedFnBody

derive instance Generic GuardedFnBody _
instance Show GuardedFnBody where
  show x = genericShow x

instance EncodeJson GuardedFnBody where
  encodeJson = genericEncodeJson

instance DecodeJson GuardedFnBody where
  decodeJson = genericDecodeJson

derive instance Eq GuardedFnBody

derive instance Generic Guard _
instance Show Guard where
  show x = genericShow x

derive instance Eq Guard

instance EncodeJson Guard where
  encodeJson = genericEncodeJson

instance DecodeJson Guard where
  decodeJson = genericDecodeJson

derive instance Generic PatternGuard _
instance Show PatternGuard where
  show x = genericShow x

derive instance Eq PatternGuard

instance EncodeJson PatternGuard where
  encodeJson = genericEncodeJson

instance DecodeJson PatternGuard where
  decodeJson = genericDecodeJson
