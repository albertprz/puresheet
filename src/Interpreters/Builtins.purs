module App.Interpreters.Builtins
  ( builtinFnsMap
  , operatorsMap
  ) where

import App.Interpreters.Object (extractList, isElement, nonNullObj)
import App.SyntaxTrees.Common (Var(..), VarOp(..))
import App.SyntaxTrees.FnDef (Arity(..), Associativity(..), BuiltinFnInfo, Object(..), OpInfo, Precedence(..))
import Bookhound.FatPrelude (elem)
import Data.Array as Array
import Data.Array.NonEmpty (toArray)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.EuclideanRing as Ring
import Data.Foldable as Foldable
import Data.Int (toNumber)
import Data.Map as Map
import Data.Semigroup.Foldable (foldl1)
import Data.Set as Set
import Data.String.CodeUnits as String
import FatPrelude (Map, Maybe(..), all, arr2, bimap, filter, fromCharArray, fromMaybe, toCharArray, traverse, ($), (&&), (*), (+), (-), (..), (/), (/=), (/\), (<), (<$>), (<..), (<=), (<>), (==), (>), (>=), (||))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

builtinFnsMap :: Map Var BuiltinFnInfo
builtinFnsMap = unsafePartial $ Map.fromFoldable
  $ bimap Var
      ( \(fn /\ arity /\ nulls) ->
          { fn, arity, defaultParams: Set.fromFoldable nulls }
      )
  <$>
    [ ("not" /\ (not /\ A1 /\ []))
    , ("neg" /\ (neg /\ A1 /\ []))
    , ("sum" /\ (sum /\ A1 /\ []))
    , ("product" /\ (product /\ A1 /\ []))
    , ("concat" /\ (concat /\ A1 /\ []))
    , ("transpose" /\ (transpose /\ A1 /\ []))
    , ("head" /\ (head /\ A1 /\ []))
    , ("tail" /\ (tail /\ A1 /\ []))
    , ("init" /\ (init /\ A1 /\ []))
    , ("last" /\ (last /\ A1 /\ []))
    , ("length" /\ (length /\ A1 /\ []))
    , ("and" /\ (and /\ A2 /\ [ 0, 1 ]))
    , ("or" /\ (or /\ A2 /\ [ 0, 1 ]))
    , ("add" /\ (add /\ A2 /\ [ 0, 1 ]))
    , ("mult" /\ (mult /\ A2 /\ [ 0, 1 ]))
    , ("gcd" /\ (gcd /\ A2 /\ [ 0, 1 ]))
    , ("lcm" /\ (lcm /\ A2 /\ [ 0, 1 ]))
    , ("append" /\ (append /\ A2 /\ [ 0, 1 ]))
    , ("sub" /\ (sub /\ A2 /\ [ 0 ]))
    , ("div" /\ (div /\ A2 /\ [ 0 ]))
    , ("mod" /\ (mod /\ A2 /\ [ 0 ]))
    , ("take" /\ (take /\ A2 /\ [ 0 ]))
    , ("takeLast" /\ (takeLast /\ A2 /\ [ 0 ]))
    , ("drop" /\ (drop /\ A2 /\ [ 0 ]))
    , ("dropLast" /\ (dropLast /\ A2 /\ [ 0 ]))
    , ("contains" /\ (contains /\ A2 /\ []))
    , ("eq" /\ (eq /\ A2 /\ []))
    , ("notEq" /\ (notEq /\ A2 /\ []))
    , ("gt" /\ (gt /\ A2 /\ []))
    , ("gtOrEq" /\ (gtOrEq /\ A2 /\ []))
    , ("lt" /\ (lt /\ A2 /\ []))
    , ("ltOrEq" /\ (ltOrEq /\ A2 /\ []))
    , ("cons" /\ (cons /\ A2 /\ []))
    , ("snoc" /\ (snoc /\ A2 /\ []))
    , ("range" /\ (range /\ A2 /\ []))
    , ("slice" /\ (slice /\ A3 /\ []))
    ]

operatorsMap :: Map VarOp OpInfo
operatorsMap = Map.fromFoldable
  $ bimap VarOp
      ( \(fnName /\ precedence /\ associativity) ->
          { fnName: Var fnName, precedence, associativity }
      )
  <$>
    [ ("||" /\ ("or" /\ P2 /\ R))
    , ("&&" /\ ("and" /\ P3 /\ R))
    , ("==" /\ ("eq" /\ P4 /\ L))
    , ("!=" /\ ("notEq" /\ P4 /\ L))
    , (">" /\ ("gt" /\ P4 /\ L))
    , (">=" /\ ("gtOrEq" /\ P4 /\ L))
    , ("<" /\ ("lt" /\ P4 /\ L))
    , ("<=" /\ ("ltOrEq" /\ P4 /\ L))
    , ("++" /\ ("append" /\ P5 /\ R))
    , ("+:" /\ ("cons" /\ P6 /\ R))
    , (":+" /\ ("snoc" /\ P7 /\ L))
    , ("+" /\ ("add" /\ P8 /\ L))
    , ("-" /\ ("sub" /\ P9 /\ L))
    , ("*" /\ ("mult" /\ P10 /\ L))
    , ("/" /\ ("div" /\ P11 /\ L))
    , ("%" /\ ("mod" /\ P11 /\ L))
    ]

-- Boolean Fns
not :: Partial => Array Object -> Object
not [ BoolObj x ] = BoolObj $ Prelude.not x

and :: Partial => Array Object -> Object
and [ BoolObj a, BoolObj b ] = BoolObj $ a && b

or :: Partial => Array Object -> Object
or [ BoolObj a, BoolObj b ] = BoolObj $ a || b

eq :: Partial => Array Object -> Object
eq [ obj1, obj2 ] = BoolObj $ obj1 == obj2

notEq :: Partial => Array Object -> Object
notEq [ obj1, obj2 ] = BoolObj $ obj1 /= obj2

gt :: Partial => Array Object -> Object
gt [ obj1, obj2 ] = BoolObj $ obj1 > obj2

gtOrEq :: Partial => Array Object -> Object
gtOrEq [ obj1, obj2 ] = BoolObj $ obj1 >= obj2

lt :: Partial => Array Object -> Object
lt [ obj1, obj2 ] = BoolObj $ obj1 < obj2

ltOrEq :: Partial => Array Object -> Object
ltOrEq [ obj1, obj2 ] = BoolObj $ obj1 <= obj2

-- Number Fns
neg :: Partial => Array Object -> Object
neg [ IntObj x ] = IntObj $ Prelude.negate x
neg [ FloatObj x ] = FloatObj $ Prelude.negate x

add :: Partial => Array Object -> Object
add [ IntObj a, IntObj b ] = IntObj $ a + b
add [ FloatObj a, FloatObj b ] = FloatObj $ a + b
add [ FloatObj a, IntObj b ] = FloatObj $ a + toNumber b
add [ IntObj a, FloatObj b ] = FloatObj $ toNumber a + b

sub :: Partial => Array Object -> Object
sub [ IntObj a, IntObj b ] = IntObj $ a - b
sub [ FloatObj a, FloatObj b ] = FloatObj $ a - b
sub [ FloatObj a, IntObj b ] = FloatObj $ a - toNumber b
sub [ IntObj a, FloatObj b ] = FloatObj $ toNumber a - b

mult :: Partial => Array Object -> Object
mult [ IntObj a, IntObj b ] = IntObj $ a * b
mult [ FloatObj a, FloatObj b ] = FloatObj $ a * b
mult [ FloatObj a, IntObj b ] = FloatObj $ a * toNumber b
mult [ IntObj a, FloatObj b ] = FloatObj $ toNumber a * b

div :: Partial => Array Object -> Object
div [ IntObj a, IntObj b ] = FloatObj $ toNumber a / toNumber b
div [ FloatObj a, FloatObj b ] = FloatObj $ a / b
div [ FloatObj a, IntObj b ] = FloatObj $ a / toNumber b
div [ IntObj a, FloatObj b ] = FloatObj $ toNumber a / b

-- Int Fns
mod :: Partial => Array Object -> Object
mod [ IntObj a, IntObj b ] = IntObj $ Ring.mod a b

gcd :: Partial => Array Object -> Object
gcd [ IntObj a, IntObj b ] = IntObj $ Ring.gcd a b

lcm :: Partial => Array Object -> Object
lcm [ IntObj a, IntObj b ] = IntObj $ Ring.lcm a b

sum :: Partial => Array Object -> Object
sum [ ListObj xs ] = foldl1 (add <.. arr2) $ NonEmptyArray $ filter nonNullObj
  xs

product :: Partial => Array Object -> Object
product [ ListObj xs ] = foldl1 (mult <.. arr2) $ NonEmptyArray $ filter
  nonNullObj
  xs

-- List / String Fns
append :: Partial => Array Object -> Object
append [ ListObj a, ListObj b ] = ListObj $ a <> b
append [ StringObj a, StringObj b ] = StringObj $ a <> b

cons :: Partial => Array Object -> Object
cons [ a, ListObj b ] = ListObj $ Array.cons a b
cons [ NullObj, ListObj a ] = ListObj $ Array.cons NullObj a
cons [ CharObj a, StringObj b ] = StringObj $ String.singleton a <> b
cons [ NullObj, StringObj a ] = StringObj a

snoc :: Partial => Array Object -> Object
snoc [ ListObj a, b ] = ListObj $ Array.snoc a b
snoc [ ListObj a, NullObj ] = ListObj $ Array.snoc a NullObj
snoc [ StringObj a, CharObj b ] = StringObj $ a <> String.singleton b
snoc [ StringObj a, NullObj ] = StringObj a

concat :: Partial => Array Object -> Object
concat [ ListObj xs ] = foldl1 (append <.. arr2) $ NonEmptyArray xs

transpose :: Partial => Array Object -> Object
transpose [ ListObj xs ] | Just xss <- traverse extractList xs =
  ListObj $ (ListObj <$> Array.transpose xss)
transpose [ ListObj xs ] | all isElement xs =
  ListObj $ (ListObj <$> Array.transpose [ xs ])

contains :: Partial => Array Object -> Object
contains [ a, ListObj b ] = BoolObj $ elem a b
contains [ CharObj a, StringObj b ] = BoolObj $ elem a (String.toCharArray b)

range :: Partial => Array Object -> Object
range [ IntObj a, IntObj b ] = ListObj $ IntObj <$> toArray (a .. b)
range [ CharObj a, CharObj b ] = ListObj $ CharObj <$> toArray (a .. b)

head :: Partial => Array Object -> Object
head [ ListObj a ] = fromMaybe NullObj $ Array.head a
head [ StringObj a ] = fromMaybe NullObj $ CharObj <$>
  (Array.head $ toCharArray a)

tail :: Partial => Array Object -> Object
tail [ ListObj a ] = fromMaybe NullObj $ ListObj <$> Array.tail a
tail [ StringObj a ] = fromMaybe NullObj $ StringObj <$> fromCharArray <$>
  (Array.tail $ toCharArray a)

last :: Partial => Array Object -> Object
last [ ListObj a ] = fromMaybe NullObj $ Array.last a
last [ StringObj a ] = fromMaybe NullObj $ CharObj <$>
  (Array.last $ toCharArray a)

init :: Partial => Array Object -> Object
init [ ListObj a ] = fromMaybe NullObj $ ListObj <$> Array.init a
init [ StringObj a ] = fromMaybe NullObj $ StringObj <$> fromCharArray <$>
  (Array.init $ toCharArray a)

length :: Partial => Array Object -> Object
length [ ListObj a ] = IntObj $ Foldable.length a
length [ StringObj a ] = IntObj $ String.length a

take :: Partial => Array Object -> Object
take [ IntObj n, ListObj xs ] = ListObj $ Array.take n xs
take [ IntObj n, StringObj xs ] = StringObj $ String.take n xs

takeLast :: Partial => Array Object -> Object
takeLast [ IntObj n, ListObj xs ] = ListObj $ Array.takeEnd n xs
takeLast [ IntObj n, StringObj xs ] = StringObj $ String.takeRight n xs

drop :: Partial => Array Object -> Object
drop [ IntObj n, ListObj xs ] = ListObj $ Array.drop n xs
drop [ IntObj n, StringObj xs ] = StringObj $ String.drop n xs

dropLast :: Partial => Array Object -> Object
dropLast [ IntObj n, ListObj xs ] = ListObj $ Array.dropEnd n xs
dropLast [ IntObj n, StringObj xs ] = StringObj $ String.dropRight n xs

slice :: Partial => Array Object -> Object
slice [ IntObj n1, IntObj n2, ListObj xs ] = ListObj $ Array.slice n1 n2 xs
slice [ IntObj n1, IntObj n2, StringObj xs ] = StringObj $ String.slice n1 n2 xs
