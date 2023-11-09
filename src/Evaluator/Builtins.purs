module App.Evaluator.Builtins
  ( builtinFnsMap
  ) where

import Prim hiding (Type)

import App.Evaluator.Object (extractList, isElement)
import App.SyntaxTree.Common (Var(..))
import App.SyntaxTree.FnDef (BuiltinFnInfo, Object(..))
import App.SyntaxTree.Type (Type(..), TypeParam(..), TypeVar(..))
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (rmap)
import Data.EuclideanRing as Ring
import Data.Int (toNumber)
import Data.Map as Map
import Data.Set as Set
import Data.String.CodeUnits as String
import Data.Tuple.Nested (type (/\))
import FatPrelude (Map, Maybe(..), all, arr2, bimap, elem, foldl1, fromCharArray, fromMaybe, toCharArray, traverse, ($), (&&), (*), (+), (-), (..), (/), (/=), (/\), (<), (<$>), (<..), (<<<), (<=), (<>), (==), (>), (>=), (||))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

builtinFnsMap :: Map Var BuiltinFnInfo
builtinFnsMap = unsafePartial $ Map.fromFoldable
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
null :: Partial => Array Object -> Object
null [] = NullObj

nullSig :: Sig
nullSig = [] /\ a

-- Boolean Fns
not :: Partial => Array Object -> Object
not [ BoolObj x ] = BoolObj $ Prelude.not x

notSig :: Sig
notSig = [ Var "x" /\ bool ] /\ bool

and :: Partial => Array Object -> Object
and [ BoolObj x, BoolObj y ] = BoolObj $ x && y

andSig :: Sig
andSig = [ Var "x" /\ bool, Var "y" /\ bool ] /\ bool

or :: Partial => Array Object -> Object
or [ BoolObj x, BoolObj y ] = BoolObj $ x || y

orSig :: Sig
orSig = [ Var "x" /\ bool, Var "y" /\ bool ] /\ bool

eq :: Partial => Array Object -> Object
eq [ x, y ] = BoolObj $ x == y

eqSig :: Sig
eqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

notEq :: Partial => Array Object -> Object
notEq [ x, y ] = BoolObj $ x /= y

notEqSig :: Sig
notEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

gt :: Partial => Array Object -> Object
gt [ x, y ] = BoolObj $ x > y

gtSig :: Sig
gtSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

gtOrEq :: Partial => Array Object -> Object
gtOrEq [ x, y ] = BoolObj $ x >= y

gtOrEqSig :: Sig
gtOrEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

lt :: Partial => Array Object -> Object
lt [ x, y ] = BoolObj $ x < y

ltSig :: Sig
ltSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

ltOrEq :: Partial => Array Object -> Object
ltOrEq [ x, y ] = BoolObj $ x <= y

ltOrEqSig :: Sig
ltOrEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

-- Number Fns
neg :: Partial => Array Object -> Object
neg [ IntObj x ] = IntObj $ Prelude.negate x
neg [ FloatObj x ] = FloatObj $ Prelude.negate x

negSig :: Sig
negSig = [ Var "x" /\ number ] /\ number

add :: Partial => Array Object -> Object
add [ IntObj x, IntObj y ] = IntObj $ x + y
add [ FloatObj x, FloatObj y ] = FloatObj $ x + y
add [ FloatObj x, IntObj y ] = FloatObj $ x + toNumber y
add [ IntObj x, FloatObj y ] = FloatObj $ toNumber x + y

addSig :: Sig
addSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

sub :: Partial => Array Object -> Object
sub [ IntObj x, IntObj y ] = IntObj $ x - y
sub [ FloatObj x, FloatObj y ] = FloatObj $ x - y
sub [ FloatObj x, IntObj y ] = FloatObj $ x - toNumber y
sub [ IntObj x, FloatObj y ] = FloatObj $ toNumber x - y

subSig :: Sig
subSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

mult :: Partial => Array Object -> Object
mult [ IntObj x, IntObj y ] = IntObj $ x * y
mult [ FloatObj x, FloatObj y ] = FloatObj $ x * y
mult [ FloatObj x, IntObj y ] = FloatObj $ x * toNumber y
mult [ IntObj x, FloatObj y ] = FloatObj $ toNumber x * y

multSig :: Sig
multSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

div :: Partial => Array Object -> Object
div [ IntObj x, IntObj y ] = FloatObj $ toNumber x / toNumber y
div [ FloatObj x, FloatObj y ] = FloatObj $ x / y
div [ FloatObj x, IntObj y ] = FloatObj $ x / toNumber y
div [ IntObj x, FloatObj y ] = FloatObj $ toNumber x / y

divSig :: Sig
divSig = [ Var "x" /\ number, Var "y" /\ number ] /\ float

-- Int Fns
mod :: Partial => Array Object -> Object
mod [ IntObj x, IntObj y ] = IntObj $ Ring.mod x y

modSig :: Sig
modSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

gcd :: Partial => Array Object -> Object
gcd [ IntObj x, IntObj y ] = IntObj $ Ring.gcd x y

gcdSig :: Sig
gcdSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

lcm :: Partial => Array Object -> Object
lcm [ IntObj x, IntObj y ] = IntObj $ Ring.lcm x y

lcmSig :: Sig
lcmSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

-- List / String Fns
append :: Partial => Array Object -> Object
append [ ArrayObj x, ArrayObj y ] = ArrayObj $ x <> y
append [ StringObj x, StringObj y ] = StringObj $ x <> y

appendSig :: Sig
appendSig = [ Var "xs" /\ arrayOf a, Var "ys" /\ arrayOf a ] /\ arrayOf a

cons :: Partial => Array Object -> Object
cons [ x, ArrayObj y ] = ArrayObj $ Array.cons x y
cons [ NullObj, ArrayObj x ] = ArrayObj $ Array.cons NullObj x
cons [ CharObj x, StringObj y ] = StringObj $ String.singleton x <> y
cons [ NullObj, StringObj x ] = StringObj x

consSig :: Sig
consSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ arrayOf a

snoc :: Partial => Array Object -> Object
snoc [ ArrayObj x, y ] = ArrayObj $ Array.snoc x y
snoc [ ArrayObj x, NullObj ] = ArrayObj $ Array.snoc x NullObj
snoc [ StringObj x, CharObj y ] = StringObj $ x <> String.singleton y
snoc [ StringObj x, NullObj ] = StringObj x

snocSig :: Sig
snocSig = [ Var "xs" /\ arrayOf a, Var "y" /\ a ] /\ arrayOf a

concat :: Partial => Array Object -> Object
concat [ ArrayObj xs ] = foldl1 (append <.. arr2) $ NonEmptyArray xs

concatSig :: Sig
concatSig = [ Var "xss" /\ (arrayOf $ arrayOf a) ] /\ arrayOf a

transpose :: Partial => Array Object -> Object
transpose [ ArrayObj xs ] | Just xss <- traverse extractList xs =
  ArrayObj $ (ArrayObj <$> Array.transpose xss)
transpose [ ArrayObj xs ] | all isElement xs =
  ArrayObj $ (ArrayObj <$> Array.transpose [ xs ])

transposeSig :: Sig
transposeSig = [ Var "xss" /\ (arrayOf $ arrayOf a) ]
  /\ (arrayOf $ arrayOf a)

contains :: Partial => Array Object -> Object
contains [ x, ArrayObj y ] = BoolObj $ elem x y
contains [ CharObj x, StringObj y ] = BoolObj $ elem x (String.toCharArray y)

containsSig :: Sig
containsSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ bool

range :: Partial => Array Object -> Object
range [ IntObj x, IntObj y ] = ArrayObj $ IntObj <$> (x .. y)
range [ CharObj x, CharObj y ] = ArrayObj $ CharObj <$> (x .. y)

rangeSig :: Sig
rangeSig = [ Var "start" /\ a, Var "end" /\ a ] /\ arrayOf a

head :: Partial => Array Object -> Object
head [ ArrayObj x ] = fromMaybe NullObj $ Array.head x
head [ StringObj x ] = fromMaybe NullObj $ CharObj <$>
  (Array.head $ toCharArray x)

headSig :: Sig
headSig = [ Var "xs" /\ arrayOf a ] /\ a

tail :: Partial => Array Object -> Object
tail [ ArrayObj x ] = fromMaybe NullObj $ ArrayObj <$> Array.tail x
tail [ StringObj x ] = fromMaybe NullObj $ StringObj <$> fromCharArray <$>
  (Array.tail $ toCharArray x)

tailSig :: Sig
tailSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

last :: Partial => Array Object -> Object
last [ ArrayObj x ] = fromMaybe NullObj $ Array.last x
last [ StringObj x ] = fromMaybe NullObj $ CharObj <$>
  (Array.last $ toCharArray x)

lastSig :: Sig
lastSig = [ Var "xs" /\ arrayOf a ] /\ a

init :: Partial => Array Object -> Object
init [ ArrayObj x ] = fromMaybe NullObj $ ArrayObj <$> Array.init x
init [ StringObj x ] = fromMaybe NullObj $ StringObj <$> fromCharArray <$>
  (Array.init $ toCharArray x)

initSig :: Sig
initSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

reverse :: Partial => Array Object -> Object
reverse [ ArrayObj xs ] = ArrayObj $ Array.reverse xs
reverse [ StringObj xs ] = StringObj $ fromCharArray $ Array.reverse $
  toCharArray xs

reverseSig :: Sig
reverseSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

length :: Partial => Array Object -> Object
length [ ArrayObj x ] = IntObj $ Array.length x
length [ StringObj x ] = IntObj $ String.length x

lengthSig :: Sig
lengthSig = [ Var "xs" /\ arrayOf a ] /\ int

take :: Partial => Array Object -> Object
take [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.take n xs
take [ IntObj n, StringObj xs ] = StringObj $ String.take n xs

takeSig :: Sig
takeSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

takeLast :: Partial => Array Object -> Object
takeLast [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.takeEnd n xs
takeLast [ IntObj n, StringObj xs ] = StringObj $ String.takeRight n xs

takeLastSig :: Sig
takeLastSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

drop :: Partial => Array Object -> Object
drop [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.drop n xs
drop [ IntObj n, StringObj xs ] = StringObj $ String.drop n xs

dropSig :: Sig
dropSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

dropLast :: Partial => Array Object -> Object
dropLast [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.dropEnd n xs
dropLast [ IntObj n, StringObj xs ] = StringObj $ String.dropRight n xs

dropLastSig :: Sig
dropLastSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

slice :: Partial => Array Object -> Object
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

typeVar :: String -> Type
typeVar = TypeVar' <<< TypeVar

typeParam :: Char -> Type
typeParam = TypeParam' <<< TypeParam

arrayOf :: Type -> Type
arrayOf = ArrayTypeApply

type Sig = Array (Var /\ Type) /\ Type
