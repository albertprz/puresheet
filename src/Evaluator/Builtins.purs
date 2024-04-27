module App.Evaluator.Builtins
  ( builtinFnsMap
  ) where

import FatPrelude hiding (add, and, append, div, elem, eq, gcd, lcm, length, mod, neg, not, notEq, null, or, sub)
import Prim hiding (Function, Type)

import App.Evaluator.Object (extractList, extractString, isElement)
import App.SyntaxTree.Common (QVar(..), Var(..), preludeModule)
import App.SyntaxTree.FnDef (BuiltinFnInfo, Object(..))
import App.SyntaxTree.Type (Type(..), TypeParam(..), TypeVar(..))
import App.Utils.String (head, init, last, tail) as String
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.EuclideanRing as Ring
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.List as List
import Data.Set as Set
import Data.String.CodeUnits (drop, dropRight, fromCharArray, length, singleton, slice, take, takeRight, toCharArray) as String
import Data.String.Utils (includes) as String
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

builtinFnsMap :: HashMap QVar BuiltinFnInfo
builtinFnsMap = unsafePartial $ HashMap.fromArray
  $
    bimap (QVar (Just preludeModule) <<< Var)
      ( \(fn /\ (params /\ returnType) /\ doc /\ nulls) ->
          { fn
          , params: rmap Just <$> params
          , returnType: Just returnType
          , defaultParams: Set.fromFoldable nulls
          , doc
          }
      )
  <$>
    [ ("isCollection") /\
        (isCollection /\ isCollectionSig /\ isCollectionDoc /\ [])
    , ("not" /\ (not /\ notSig /\ notDoc /\ []))
    , ("neg" /\ (neg /\ negSig /\ negDoc /\ []))
    , ("concat" /\ (concat /\ concatSig /\ concatDoc /\ []))
    , ("transpose" /\ (transpose /\ transposeSig /\ transposeDoc /\ []))
    , ("head" /\ (head /\ headSig /\ headDoc /\ []))
    , ("tail" /\ (tail /\ tailSig /\ tailDoc /\ []))
    , ("init" /\ (init /\ initSig /\ initDoc /\ []))
    , ("last" /\ (last /\ lastSig /\ lastDoc /\ []))
    , ("reverse" /\ (reverse /\ reverseSig /\ reverseDoc /\ []))
    , ("length" /\ (length /\ lengthSig /\ lengthDoc /\ []))
    , ("and" /\ (and /\ andSig /\ andDoc /\ [ 0, 1 ]))
    , ("or" /\ (or /\ orSig /\ orDoc /\ [ 0, 1 ]))
    , ("add" /\ (add /\ addSig /\ addDoc /\ [ 0, 1 ]))
    , ("mult" /\ (mult /\ multSig /\ multDoc /\ [ 0, 1 ]))
    , ("gcd" /\ (gcd /\ gcdSig /\ gcdDoc /\ [ 0, 1 ]))
    , ("lcm" /\ (lcm /\ lcmSig /\ lcmDoc /\ [ 0, 1 ]))
    , ("append" /\ (append /\ appendSig /\ appendDoc /\ [ 0, 1 ]))
    , ("sub" /\ (sub /\ subSig /\ subDoc /\ [ 0 ]))
    , ("div" /\ (div /\ divSig /\ divDoc /\ [ 0 ]))
    , ("intDiv" /\ (intDiv /\ intDivSig /\ intDivDoc /\ [ 0 ]))
    , ("mod" /\ (mod /\ modSig /\ modDoc /\ [ 0 ]))
    , ("take" /\ (take /\ takeSig /\ takeDoc /\ [ 0 ]))
    , ("takeLast" /\ (takeLast /\ takeLastSig /\ takeLastDoc /\ [ 0 ]))
    , ("drop" /\ (drop /\ dropSig /\ dropDoc /\ [ 0 ]))
    , ("dropLast" /\ (dropLast /\ dropLastSig /\ dropLastDoc /\ [ 0 ]))
    , ("elem" /\ (elem /\ elemSig /\ elemDoc /\ []))
    , ("contains" /\ (contains /\ containsSig /\ containsDoc /\ []))
    , ("eq" /\ (eq /\ eqSig /\ eqDoc /\ []))
    , ("notEq" /\ (notEq /\ notEqSig /\ notEqDoc /\ []))
    , ("gt" /\ (gt /\ gtSig /\ gtDoc /\ []))
    , ("gtOrEq" /\ (gtOrEq /\ gtOrEqSig /\ gtOrEqDoc /\ []))
    , ("lt" /\ (lt /\ ltSig /\ ltDoc /\ []))
    , ("ltOrEq" /\ (ltOrEq /\ ltOrEqSig /\ ltOrEqDoc /\ []))
    , ("cons" /\ (cons /\ consSig /\ consDoc /\ []))
    , ("snoc" /\ (snoc /\ snocSig /\ snocDoc /\ []))
    , ("range" /\ (range /\ rangeSig /\ rangeDoc /\ []))
    , ("slice" /\ (slice /\ sliceSig /\ sliceDoc /\ []))
    ]

-- Type check Fns

isCollection :: Function
isCollection [ ArrayObj _ ] = BoolObj true
isCollection [ ListObj _ ] = BoolObj true
isCollection [ StringObj _ ] = BoolObj true
isCollection _ = BoolObj false

isCollectionSig :: Sig
isCollectionSig = [ Var "x" /\ a ] /\ bool

isCollectionDoc :: String
isCollectionDoc =
  """Checks if the argument is a collection of values
 >>> [1, 2, 4]
 >>> 1
"""

-- Boolean Fns
not :: Function
not [ BoolObj x ] = BoolObj $ Prelude.not x

notSig :: Sig
notSig = [ Var "x" /\ bool ] /\ bool

notDoc :: String
notDoc =
  """Negates a boolean condition
 >>> true
 >>> false
"""

and :: Function
and [ BoolObj x, BoolObj y ] = BoolObj $ x && y

andSig :: Sig
andSig = [ Var "x" /\ bool, Var "y" /\ bool ] /\ bool

andDoc :: String
andDoc =
  """Logical conjuction of two boolean conditions
 >>> true, true
 >>> true, false
"""

or :: Function
or [ BoolObj x, BoolObj y ] = BoolObj $ x || y

orSig :: Sig
orSig = [ Var "x" /\ bool, Var "y" /\ bool ] /\ bool

orDoc :: String
orDoc =
  """Logical disjunction of two boolean conditions
 >>> true, false
 >>> false, false
"""

eq :: Function
eq [ x, y ] = BoolObj $ x == y

eqSig :: Sig
eqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

eqDoc :: String
eqDoc =
  """Equality check between two values
 >>> [1, 2, 3], [1, 2, 3]
 >>> "asdf", "fdsa"
"""

notEq :: Function
notEq [ x, y ] = BoolObj $ x /= y

notEqSig :: Sig
notEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

notEqDoc :: String
notEqDoc =
  """Inequality check between two values
 >>> [1, 2, 3], [1, 2, 3]
 >>> "asdf", "fdsa"
"""

gt :: Function
gt [ x, y ] = BoolObj $ x > y

gtSig :: Sig
gtSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

gtDoc :: String
gtDoc =
  """Checks if the first argument is greater than the second one
 >>> 6.2, 3.75
 >>> "abc", "def"
"""

gtOrEq :: Function
gtOrEq [ x, y ] = BoolObj $ x >= y

gtOrEqSig :: Sig
gtOrEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

gtOrEqDoc :: String
gtOrEqDoc =
  """Checks if the first argument is greater than or equal to the second one
 >>> 6, 6
 >>> "abc", "def"
"""

lt :: Function
lt [ x, y ] = BoolObj $ x < y

ltSig :: Sig
ltSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

ltDoc :: String
ltDoc =
  """Checks if the first argument is lesser than the second one
 >>> 6.2, 3.75
 >>> "abc", "def"
"""

ltOrEq :: Function
ltOrEq [ x, y ] = BoolObj $ x <= y

ltOrEqSig :: Sig
ltOrEqSig = [ Var "x" /\ a, Var "y" /\ a ] /\ bool

ltOrEqDoc :: String
ltOrEqDoc =
  """Checks if the first argument is lesser than or equal to the second one
 >>> 6, 6
 >>> "abc", "def"
"""

-- Number Fns
neg :: Function
neg [ IntObj x ] = IntObj $ Prelude.negate x
neg [ FloatObj x ] = FloatObj $ Prelude.negate x

negSig :: Sig
negSig = [ Var "x" /\ number ] /\ number

negDoc :: String
negDoc =
  """Negates a number
 >>> 78
 >>> -5.4
"""

add :: Function
add [ IntObj x, IntObj y ] = IntObj $ x + y
add [ FloatObj x, FloatObj y ] = FloatObj $ x + y
add [ FloatObj x, IntObj y ] = FloatObj $ x + toNumber y
add [ IntObj x, FloatObj y ] = FloatObj $ toNumber x + y

addSig :: Sig
addSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

addDoc :: String
addDoc =
  """Adds two numbers
 >>> 1, 4
 >>> 3.9, 5.1
"""

sub :: Function
sub [ IntObj x, IntObj y ] = IntObj $ x - y
sub [ FloatObj x, FloatObj y ] = FloatObj $ x - y
sub [ FloatObj x, IntObj y ] = FloatObj $ x - toNumber y
sub [ IntObj x, FloatObj y ] = FloatObj $ toNumber x - y

subSig :: Sig
subSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

subDoc :: String
subDoc =
  """Substracts the second number from the first one
 >>> 1, 4
 >>> 3.9, 5.1
"""

mult :: Function
mult [ IntObj x, IntObj y ] = IntObj $ x * y
mult [ FloatObj x, FloatObj y ] = FloatObj $ x * y
mult [ FloatObj x, IntObj y ] = FloatObj $ x * toNumber y
mult [ IntObj x, FloatObj y ] = FloatObj $ toNumber x * y

multSig :: Sig
multSig = [ Var "x" /\ number, Var "y" /\ number ] /\ number

multDoc :: String
multDoc =
  """Multiplies two numbers
 >>> 2, 7
 >>> -2.3, 4.8
"""

div :: Function
div [ IntObj x, IntObj y ] = FloatObj $ toNumber x / toNumber y
div [ FloatObj x, FloatObj y ] = FloatObj $ x / y
div [ FloatObj x, IntObj y ] = FloatObj $ x / toNumber y
div [ IntObj x, FloatObj y ] = FloatObj $ toNumber x / y

divSig :: Sig
divSig = [ Var "x" /\ number, Var "y" /\ number ] /\ float

divDoc :: String
divDoc =
  """Divides the first number by the second one
 >>> 2, 7
 >>> -2.3, 4.8
"""

-- Int Fns
intDiv :: Function
intDiv [ IntObj x, IntObj y ] = IntObj $ Ring.div x y

intDivSig :: Sig
intDivSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

intDivDoc :: String
intDivDoc =
  """Divides the first integer by the second one
 >>> 8, 2
 >>> 5, 3
"""

mod :: Function
mod [ IntObj x, IntObj y ] = IntObj $ Ring.mod x y

modSig :: Sig
modSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

modDoc :: String
modDoc =
  """Calculates the modulus of the first integer by the second one
 >>> 8, 2
 >>> 5, 3
"""

gcd :: Function
gcd [ IntObj x, IntObj y ] = IntObj $ Ring.gcd x y

gcdSig :: Sig
gcdSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

gcdDoc :: String
gcdDoc =
  """Calculates the greatest common divisor between two integers
 >>> 8, 2
 >>> 5, 3
"""

lcm :: Function
lcm [ IntObj x, IntObj y ] = IntObj $ Ring.lcm x y

lcmSig :: Sig
lcmSig = [ Var "x" /\ int, Var "y" /\ int ] /\ int

lcmDoc :: String
lcmDoc =
  """Calculates the least common multiplier between two integers
 >>> 8, 2
 >>> 5, 3
"""

-- List / String Fns
append :: Function
append [ ListObj x, ListObj y ] = ListObj $ x <> y
append [ ArrayObj x, ArrayObj y ] = ArrayObj $ x <> y
append [ StringObj x, StringObj y ] = StringObj $ x <> y

appendSig :: Sig
appendSig = [ Var "xs" /\ arrayOf a, Var "ys" /\ arrayOf a ] /\ arrayOf a

appendDoc :: String
appendDoc =
  """Appends two collections
 >>> [1, 2], [3]
 >>> "ab", "c"
"""

cons :: Function
cons [ x, ListObj y ] = ListObj $ Cons x y
cons [ NullObj, ListObj x ] = ListObj $ Cons NullObj x
cons [ x, ArrayObj y ] = cons [ x, ListObj $ List.fromFoldable y ]
cons [ NullObj, ArrayObj x ] = cons [ NullObj, ListObj $ List.fromFoldable x ]
cons [ CharObj x, StringObj y ] = StringObj $ String.singleton x <> y
cons [ NullObj, StringObj x ] = StringObj x

consSig :: Sig
consSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ arrayOf a

consDoc :: String
consDoc =
  """Prepends an element to the start of the a collection
 >>> 1, [2, 3]
 >>> 'a', "bc"
"""

snoc :: Function
snoc [ ListObj x, y ] = ListObj $ List.snoc x y
snoc [ ListObj x, NullObj ] = ListObj $ List.snoc x NullObj
snoc [ ArrayObj x, y ] = ArrayObj $ Array.snoc x y
snoc [ ArrayObj x, NullObj ] = ArrayObj $ Array.snoc x NullObj
snoc [ StringObj x, CharObj y ] = StringObj $ x <> String.singleton y
snoc [ StringObj x, NullObj ] = StringObj x

snocSig :: Sig
snocSig = [ Var "xs" /\ arrayOf a, Var "y" /\ a ] /\ arrayOf a

snocDoc :: String
snocDoc =
  """Appends an element to the end of a collection
 >>> [1, 2], 3
 >>> "ab", 'c'
"""

concat :: Function
concat [ ListObj xs ] = concat [ ArrayObj $ Array.fromFoldable xs ]
concat [ ArrayObj xs ]
  | Just xss <- traverse extractList xs =
      ArrayObj $ Array.concat xss
  | Just strs <- traverse extractString xs =
      StringObj $ fold strs

concatSig :: Sig
concatSig = [ Var "xss" /\ (arrayOf $ arrayOf a) ] /\ arrayOf a

concatDoc :: String
concatDoc =
  """Concats a matrix of values into a flat collection
 >>> [[1, 2], [3, 4, 5], [6, 7]]
 >>> ["abc", "de", "fg"]
"""

transpose :: Function
transpose [ ListObj xs ] =
  transpose [ ArrayObj $ Array.fromFoldable xs ]
transpose [ ArrayObj xs ]
  | Just xss <- traverse extractList xs =
      ArrayObj $ (ArrayObj <$> Array.transpose xss)
  | Just strs <- traverse extractString xs =
      ArrayObj
        $ map (StringObj <<< String.fromCharArray)
        $ Array.transpose
        $ map String.toCharArray strs
  | all isElement xs =
      ArrayObj $ (ArrayObj <$> Array.transpose [ xs ])

transposeSig :: Sig
transposeSig = [ Var "xss" /\ (arrayOf $ arrayOf a) ]
  /\ (arrayOf $ arrayOf a)

transposeDoc :: String
transposeDoc =
  """Transposes a matrix of values
 >>> [[1, 2], [3, 4], [5, 6]]
 >>> ["ab", "cd", "ef"]
"""

elem :: Function
elem [ x, ListObj y ] = BoolObj $ List.elem x y
elem [ x, ArrayObj y ] = BoolObj $ Array.elem x y
elem [ CharObj x, StringObj y ] = BoolObj $ Array.elem x (String.toCharArray y)

elemSig :: Sig
elemSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ bool

elemDoc :: String
elemDoc =
  """Checks if the first argument is an element in the collection
 >>> [1, 2, 3], [[6], [1, 2, 3], 8]
 >>> 'i', "hello"
"""

contains :: Function
contains [ x, y ]
  | Just xs /\ Just ys <- extractList x /\ extractList y = BoolObj
      $ Set.subset (Set.fromFoldable xs) (Set.fromFoldable ys)
contains [ StringObj x, StringObj y ] = BoolObj $ String.includes x y

containsSig :: Sig
containsSig = [ Var "x" /\ a, Var "ys" /\ arrayOf a ] /\ bool

containsDoc :: String
containsDoc =
  """Checks if the first collection is included in the second one
 >>> [1, 2, 3], [1, 2, 3, 4, 5]
 >>> "i", "hello"
"""

range :: Function
range [ IntObj x, IntObj y ] = ArrayObj $ IntObj <$> NonEmptyArray.toArray
  (x .. y)
range [ CharObj x, CharObj y ] = ArrayObj $ CharObj <$> NonEmptyArray.toArray
  (x .. y)

rangeSig :: Sig
rangeSig = [ Var "start" /\ a, Var "end" /\ a ] /\ arrayOf a

rangeDoc :: String
rangeDoc =
  """Generates a range from start to end of an enumerable type
 >>> 1, 8
 >>> 'a', 'f'
"""

head :: Function
head [ ListObj x ] = fromMaybe NullObj $ List.head x
head [ ArrayObj x ] = fromMaybe NullObj $ Array.head x
head [ StringObj x ] = fromMaybe NullObj $ CharObj <$> String.head x

headSig :: Sig
headSig = [ Var "xs" /\ arrayOf a ] /\ a

headDoc :: String
headDoc =
  """Returns the first element from the collection
 >>> [1.1, 2.2, 3.3]
 >>> "hello"
"""

tail :: Function
tail [ ListObj x ] = ListObj $ fold $ List.tail x
tail [ ArrayObj x ] = tail [ ListObj $ List.fromFoldable x ]
tail [ StringObj x ] = StringObj $ String.tail x

tailSig :: Sig
tailSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

tailDoc :: String
tailDoc =
  """Returns all elements but the first from the collection
 >>> [1.1, 2.2, 3.3]
 >>> "hello"
"""

last :: Function
last [ ListObj x ] = fromMaybe NullObj $ List.last x
last [ ArrayObj x ] = fromMaybe NullObj $ Array.last x
last [ StringObj x ] = fromMaybe NullObj $ CharObj <$> String.last x

lastSig :: Sig
lastSig = [ Var "xs" /\ arrayOf a ] /\ a

lastDoc :: String
lastDoc =
  """Returns the last element from the collection
 >>> [1.1, 2.2, 3.3]
 >>> "hello"
"""

init :: Function
init [ ListObj x ] = ListObj $ fold $ List.init x
init [ ArrayObj x ] = ArrayObj $ fold $ Array.init x
init [ StringObj x ] = StringObj $ String.init x

initSig :: Sig
initSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

initDoc :: String
initDoc =
  """Returns all elements but the last from the collection
 >>> [1.1, 2.2, 3.3]
 >>> "hello"
"""

reverse :: Function
reverse [ ListObj x ] = reverse [ ArrayObj $ Array.fromFoldable x ]
reverse [ ArrayObj x ] = ArrayObj $ Array.reverse x
reverse [ StringObj x ] = StringObj $ fromCharArray $ Array.reverse $
  toCharArray x

reverseSig :: Sig
reverseSig = [ Var "xs" /\ arrayOf a ] /\ arrayOf a

reverseDoc :: String
reverseDoc =
  """Reverses the order of a collection
 >>> [1.1, 2.2, 3.3]
 >>> "hello"
"""

length :: Function
length [ ListObj x ] = IntObj $ List.length x
length [ ArrayObj x ] = IntObj $ Array.length x
length [ StringObj x ] = IntObj $ String.length x

lengthSig :: Sig
lengthSig = [ Var "xs" /\ arrayOf a ] /\ int

lengthDoc :: String
lengthDoc =
  """Returns the length of the collection
 >>> [1.1, 2.2, 3.3]
 >>> "hello"
"""

take :: Function
take [ IntObj n, ListObj xs ] = ListObj $ List.take n xs
take [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.take n xs
take [ IntObj n, StringObj xs ] = StringObj $ String.take n xs

takeSig :: Sig
takeSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

takeDoc :: String
takeDoc =
  """Returns the first n elements from the collection
 >>> 0, [1.1, 2.2, 3.3]
 >>> 2, "hello"
"""

takeLast :: Function
takeLast [ IntObj n, ListObj xs ] = ListObj $ List.takeEnd n xs
takeLast [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.takeEnd n xs
takeLast [ IntObj n, StringObj xs ] = StringObj $ String.takeRight n xs

takeLastSig :: Sig
takeLastSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

takeLastDoc :: String
takeLastDoc =
  """Returns the last n elements from the collection
 >>> 0, [1.1, 2.2, 3.3]
 >>> 2, "hello"
"""

drop :: Function
drop [ IntObj n, ListObj xs ] = ListObj $ List.drop n xs
drop [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.drop n xs
drop [ IntObj n, StringObj xs ] = StringObj $ String.drop n xs

dropSig :: Sig
dropSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

dropDoc :: String
dropDoc =
  """Returns all elements except for the first n from the collection
 >>> 0, [1.1, 2.2, 3.3]
 >>> 2, "hello"
"""

dropLast :: Function
dropLast [ IntObj n, ListObj xs ] = ListObj $ List.dropEnd n xs
dropLast [ IntObj n, ArrayObj xs ] = ArrayObj $ Array.dropEnd n xs
dropLast [ IntObj n, StringObj xs ] = StringObj $ String.dropRight n xs

dropLastSig :: Sig
dropLastSig = [ Var "n" /\ int, Var "xs" /\ arrayOf a ] /\ arrayOf a

dropLastDoc :: String
dropLastDoc =
  """Returns all elements except for the last n from the collection
 >>> 0, [1.1, 2.2, 3.3]
 >>> 2, "hello"
"""

slice :: Function
slice [ IntObj n1, IntObj n2, ListObj xs ] =
  ListObj $ List.slice n1 n2 xs
slice [ IntObj n1, IntObj n2, ArrayObj xs ] =
  ArrayObj $ Array.slice n1 n2 xs
slice [ IntObj n1, IntObj n2, StringObj xs ] =
  StringObj $ String.slice n1 n2 xs

sliceSig :: Sig
sliceSig = [ Var "start" /\ int, Var "end" /\ int, Var "xs" /\ arrayOf a ]
  /\ arrayOf a

sliceDoc :: String
sliceDoc =
  """Returns a slice from the collection between the given start and end indexes
 >>> 0, 1, [1.1, 2.2, 3.3]
 >>> 2, 4, "hello"
"""

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

type Function = Partial => Array Object -> Object
type Sig = Array (Var /\ Type) /\ Type
