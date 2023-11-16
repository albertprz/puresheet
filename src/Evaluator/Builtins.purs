module App.Evaluator.Builtins
  ( builtinFnsMap
  ) where

import Prim hiding (Type, Function)

import App.Evaluator.Object (extractList, isElement)
import App.SyntaxTree.Common (Var(..))
import App.SyntaxTree.FnDef (BuiltinFnInfo, Object(..))
import App.SyntaxTree.Type (Type(..), TypeParam(..), TypeVar(..))
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (rmap)
import Data.EuclideanRing as Ring
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.Set as Set
import Data.String.CodeUnits as String
import Data.Tuple.Nested (type (/\))
import FatPrelude (HashMap, Maybe(..), all, arr2, bimap, elem, foldl1, fromCharArray, fromMaybe, toCharArray, traverse, ($), (&&), (*), (+), (-), (..), (/), (/=), (/\), (<), (<$>), (<..), (<<<), (<=), (<>), (==), (>), (>=), (||))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

builtinFnsMap :: HashMap Var BuiltinFnInfo
builtinFnsMap = unsafePartial $ HashMap.fromArray
  $
    bimap Var
      ( \(fn /\ (params /\ returnType) /\ nulls) ->
          { fn
          , params: rmap Just <$> params
          , returnType: Just returnType
          , defaultParams: Set.fromFoldable nulls
          }
      )
  <$>
    [ ("null" /\ (null /\ nullSig /\ []))
    , ("isArray") /\ (isArray /\ isArraySig /\ [])
    , ("isNull") /\ (isNull /\ isNullSig /\ [])
    , ("not" /\ (not /\ notSig /\ []))
    , ("neg" /\ (neg /\ negSig /\ []))
    , ("concat" /\ (concat /\ concatSig /\ []))
    , ("transpose" /\ (transpose /\ transposeSig /\ []))
    , ("head" /\ (head /\ headSig /\ []))
    , ("tail" /\ (tail /\ tailSig /\ []))
    , ("init" /\ (init /\ initSig /\ []))
    , ("last" /\ (last /\ lastSig /\ []))
    , ("reverse" /\ (reverse /\ reverseSig /\ []))
    , ("length" /\ (length /\ lengthSig /\ []))
    , ("and" /\ (and /\ andSig /\ [ 0, 1 ]))
    , ("or" /\ (or /\ orSig /\ [ 0, 1 ]))
    , ("add" /\ (add /\ addSig /\ [ 0, 1 ]))
    , ("mult" /\ (mult /\ multSig /\ [ 0, 1 ]))
    , ("gcd" /\ (gcd /\ gcdSig /\ [ 0, 1 ]))
    , ("lcm" /\ (lcm /\ lcmSig /\ [ 0, 1 ]))
    , ("append" /\ (append /\ appendSig /\ [ 0, 1 ]))
    , ("sub" /\ (sub /\ subSig /\ [ 0 ]))
    , ("div" /\ (div /\ divSig /\ [ 0 ]))
    , ("mod" /\ (mod /\ modSig /\ [ 0 ]))
    , ("take" /\ (take /\ takeSig /\ [ 0 ]))
    , ("takeLast" /\ (takeLast /\ takeLastSig /\ [ 0 ]))
    , ("drop" /\ (drop /\ dropSig /\ [ 0 ]))
    , ("dropLast" /\ (dropLast /\ dropLastSig /\ [ 0 ]))
    , ("contains" /\ (contains /\ containsSig /\ []))
    , ("eq" /\ (eq /\ eqSig /\ []))
    , ("notEq" /\ (notEq /\ notEqSig /\ []))
    , ("gt" /\ (gt /\ gtSig /\ []))
    , ("gtOrEq" /\ (gtOrEq /\ gtOrEqSig /\ []))
    , ("lt" /\ (lt /\ ltSig /\ []))
    , ("ltOrEq" /\ (ltOrEq /\ ltOrEqSig /\ []))
    , ("cons" /\ (cons /\ consSig /\ []))
    , ("snoc" /\ (snoc /\ snocSig /\ []))
    , ("range" /\ (range /\ rangeSig /\ []))
    , ("slice" /\ (slice /\ sliceSig /\ []))
    ]

-- Null
null :: Function
null [] = NullObj

nullSig :: Sig
nullSig = [] /\ a

-- Type check Fns
isArray :: Function
isArray [ArrayObj _] = BoolObj true
isArray _ = BoolObj false

isArraySig :: Sig
isArraySig = [ Var "x" /\ object ] /\ bool

isNull :: Function
isNull [NullObj] = BoolObj true
isNull _ = BoolObj false

isNullSig :: Sig
isNullSig = [ Var "x" /\ object ] /\ bool


-- Boolean Fns
not :: Function
not [ BoolObj x ] = BoolObj $ Prelude.not x

notSig :: Sig
notSig = [ Var "x" /\ bool ] /\ bool

and :: Function
and [ BoolObj x, BoolObj y ] = BoolObj $ x && y

andSig :: Sig
andSig = [ Var "x" /\ bool, Var "y" /\ bool ] /\ bool

or :: Function
or [ BoolObj x, BoolObj y ] = BoolObj $ x || y

orSig :: Sig
orSig = [ Var "x" /\ bool, Var "y" /\ bool ] /\ bool

eq :: Function
eq [ x, y ] = BoolObj $ x == y

eqSig :: Sig
eqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

notEq :: Function
notEq [ x, y ] = BoolObj $ x /= y

notEqSig :: Sig
notEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

gt :: Function
gt [ x, y ] = BoolObj $ x > y

gtSig :: Sig
gtSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

gtOrEq :: Function
gtOrEq [ x, y ] = BoolObj $ x >= y

gtOrEqSig :: Sig
gtOrEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

lt :: Function
lt [ x, y ] = BoolObj $ x < y

ltSig :: Sig
ltSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

ltOrEq :: Function
ltOrEq [ x, y ] = BoolObj $ x <= y

ltOrEqSig :: Sig
ltOrEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

-- Number Fns
neg :: Function
neg [ IntObj x ] = IntObj $ Prelude.negate x
neg [ FloatObj x ] = FloatObj $ Prelude.negate x

negSig :: Sig
negSig = [ Var "x" /\ number ] /\ number

add :: Function
add [ IntObj x, IntObj y ] = IntObj $ x + y
add [ FloatObj x, FloatObj y ] = FloatObj $ x + y
add [ FloatObj x, IntObj y ] = FloatObj $ x + toNumber y
add [ IntObj x, FloatObj y ] = FloatObj $ toNumber x + y

addSig :: Sig
addSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

sub :: Function
sub [ IntObj x, IntObj y ] = IntObj $ x - y
sub [ FloatObj x, FloatObj y ] = FloatObj $ x - y
sub [ FloatObj x, IntObj y ] = FloatObj $ x - toNumber y
sub [ IntObj x, FloatObj y ] = FloatObj $ toNumber x - y

subSig :: Sig
subSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

mult :: Function
mult [ IntObj x, IntObj y ] = IntObj $ x * y
mult [ FloatObj x, FloatObj y ] = FloatObj $ x * y
mult [ FloatObj x, IntObj y ] = FloatObj $ x * toNumber y
mult [ IntObj x, FloatObj y ] = FloatObj $ toNumber x * y

multSig :: Sig
multSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

div :: Function
div [ IntObj x, IntObj y ] = FloatObj $ toNumber x / toNumber y
div [ FloatObj x, FloatObj y ] = FloatObj $ x / y
div [ FloatObj x, IntObj y ] = FloatObj $ x / toNumber y
div [ IntObj x, FloatObj y ] = FloatObj $ toNumber x / y

divSig :: Sig
divSig = [ Var "x" /\ number, Var "y" /\ number ] /\ float

-- Int Fns
mod :: Function
mod [ IntObj x, IntObj y ] = IntObj $ Ring.mod x y

modSig :: Sig
modSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

gcd :: Function
gcd [ IntObj x, IntObj y ] = IntObj $ Ring.gcd x y

gcdSig :: Sig
gcdSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

lcm :: Function
lcm [ IntObj x, IntObj y ] = IntObj $ Ring.lcm x y

lcmSig :: Sig
lcmSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

-- List / String Fns
append :: Function
append [ ArrayObj x, ArrayObj y ] = ArrayObj $ x <> y
append [ StringObj x, StringObj y ] = StringObj $ x <> y

appendSig :: Sig
appendSig = [ Var "xs" /\ arrayOf a, Var "ys" /\ arrayOf a ] /\ arrayOf a

cons :: Function
cons [ x, ArrayObj y ] = ArrayObj $ Array.cons x y
cons [ NullObj, ArrayObj x ] = ArrayObj $ Array.cons NullObj x
cons [ CharObj x, StringObj y ] = StringObj $ String.singleton x <> y
cons [ NullObj, StringObj x ] = StringObj x

consSig :: Sig
consSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ arrayOf a

snoc :: Function
snoc [ ArrayObj x, y ] = ArrayObj $ Array.snoc x y
snoc [ ArrayObj x, NullObj ] = ArrayObj $ Array.snoc x NullObj
snoc [ StringObj x, CharObj y ] = StringObj $ x <> String.singleton y
snoc [ StringObj x, NullObj ] = StringObj x

snocSig :: Sig
snocSig = [ Var "xs" /\ arrayOf a, Var "y" /\ a ] /\ arrayOf a

concat :: Function
concat [ ArrayObj xs ] = foldl1 (append <.. arr2) $ NonEmptyArray xs

concatSig :: Sig
concatSig = [ Var "xss" /\ (arrayOf $ arrayOf a) ] /\ arrayOf a

transpose :: Function
transpose [ ArrayObj xs ] | Just xss <- traverse extractList xs =
  ArrayObj $ (ArrayObj <$> Array.transpose xss)
transpose [ ArrayObj xs ] | all isElement xs =
  ArrayObj $ (ArrayObj <$> Array.transpose [ xs ])

transposeSig :: Sig
transposeSig = [ Var "xss" /\ (arrayOf $ arrayOf a) ]
  /\ (arrayOf $ arrayOf a)

contains :: Function
contains [ x, ArrayObj y ] = BoolObj $ elem x y
contains [ CharObj x, StringObj y ] = BoolObj $ elem x (String.toCharArray y)

containsSig :: Sig
containsSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ bool

range :: Function
range [ IntObj x, IntObj y ] = ArrayObj $ IntObj <$> (x .. y)
range [ CharObj x, CharObj y ] = ArrayObj $ CharObj <$> (x .. y)

rangeSig :: Sig
rangeSig = [ Var "start" /\ a, Var "end" /\ a ] /\ arrayOf a

head :: Function
head [ ArrayObj x ] = fromMaybe NullObj $ Array.head x
head [ StringObj x ] = fromMaybe NullObj $ CharObj <$>
  (Array.head $ toCharArray x)

headSig :: Sig
headSig = [ Var "xs" /\ arrayOf a ] /\ a

tail :: Function
tail [ ArrayObj x ] = fromMaybe NullObj $ ArrayObj <$> Array.tail x
tail [ StringObj x ] = fromMaybe NullObj $ StringObj <$> fromCharArray <$>
  (Array.tail $ toCharArray x)

tailSig :: Sig
tailSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

last :: Function
last [ ArrayObj x ] = fromMaybe NullObj $ Array.last x
last [ StringObj x ] = fromMaybe NullObj $ CharObj <$>
  (Array.last $ toCharArray x)

lastSig :: Sig
lastSig = [ Var "xs" /\ arrayOf a ] /\ a

init :: Function
init [ ArrayObj x ] = fromMaybe NullObj $ ArrayObj <$> Array.init x
init [ StringObj x ] = fromMaybe NullObj $ StringObj <$> fromCharArray <$>
  (Array.init $ toCharArray x)

initSig :: Sig
initSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

reverse :: Function
reverse [ ArrayObj xs ] = ArrayObj $ Array.reverse xs
reverse [ StringObj xs ] = StringObj $ fromCharArray $ Array.reverse $
  toCharArray xs

reverseSig :: Sig
reverseSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

length :: Function
length [ ArrayObj x ] = IntObj $ Array.length x
length [ StringObj x ] = IntObj $ String.length x

lengthSig :: Sig
lengthSig = [ Var "xs" /\ arrayOf a ] /\ int

take :: Function
take [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.take n xs
take [ IntObj n, StringObj xs ] = StringObj $ String.take n xs

takeSig :: Sig
takeSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

takeLast :: Function
takeLast [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.takeEnd n xs
takeLast [ IntObj n, StringObj xs ] = StringObj $ String.takeRight n xs

takeLastSig :: Sig
takeLastSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

drop :: Function
drop [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.drop n xs
drop [ IntObj n, StringObj xs ] = StringObj $ String.drop n xs

dropSig :: Sig
dropSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

dropLast :: Function
dropLast [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.dropEnd n xs
dropLast [ IntObj n, StringObj xs ] = StringObj $ String.dropRight n xs

dropLastSig :: Sig
dropLastSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

slice :: Function
slice [ IntObj n1, IntObj n2, ArrayObj xs ] =
  ArrayObj $ Array.slice n1 n2 xs
slice [ IntObj n1, IntObj n2, StringObj xs ] =
  StringObj $ String.slice n1 n2 xs

sliceSig :: Sig
sliceSig = [ Var "start" /\ int, Var "end" /\ int, Var "xs" /\ arrayOf a ]
  /\ arrayOf a

a :: Type
a = typeParam 'A'

bool :: Type
bool = typeVar "Boolean"

int :: Type
int = typeVar "Int"

number :: Type
number = typeVar "Number"

float :: Type
float = typeVar "Float"

object :: Type
object = typeVar "Object"

typeVar :: String -> Type
typeVar = TypeVar' <<< TypeVar

typeParam :: Char -> Type
typeParam = TypeParam' <<< TypeParam

arrayOf :: Type -> Type
arrayOf = ArrayTypeApply

type Function = Partial => Array Object -> Object
type Sig = Array (Var /\ Type) /\ Type
