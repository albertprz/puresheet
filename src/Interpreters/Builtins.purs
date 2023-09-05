module App.Interpreters.Builtins
  ( builtinFnsMap
  , operatorsMap
  ) where

import App.SyntaxTrees.FnDef

import App.SyntaxTrees.Common (Var(..), VarOp(..))
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.EuclideanRing as Ring
import Data.Map as Map
import Data.Semigroup.Foldable (foldl1)
import Data.String.CodeUnits as String
import FatPrelude (type (/\), Map, bimap, lmap, ($), (&&), (*), (+), (-), (/), (/\), (<$>), (<..), (<>), (||))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

builtinFnsMap :: Map Var ((Array Object -> Object) /\ Int)
builtinFnsMap = unsafePartial $ Map.fromFoldable $ lmap Var <$>
  [ ("not" /\ (not /\ 1))
  , ("neg" /\ (neg /\ 1))
  , ("sum" /\ (sum /\ 1))
  , ("product" /\ (product /\ 1))
  , ("concat" /\ (concat /\ 1))
  , ("and" /\ (and /\ 2))
  , ("or" /\ (or /\ 2))
  , ("add" /\ (add /\ 2))
  , ("sub" /\ (sub /\ 2))
  , ("mult" /\ (mult /\ 2))
  , ("div" /\ (div /\ 2))
  , ("mod" /\ (mod /\ 2))
  , ("gcd" /\ (gcd /\ 2))
  , ("lcm" /\ (lcm /\ 2))
  , ("append" /\ (append /\ 2))
  , ("cons" /\ (cons /\ 2))
  , ("snoc" /\ (snoc /\ 2))
  ]

operatorsMap :: Map VarOp OpInfo
operatorsMap = Map.fromFoldable
  $ bimap VarOp
      ( \(fnName /\ precedence /\ associativity) ->
          { fnName: Var fnName, precedence, associativity }
      )
  <$>
    [ ("||" /\ ("or" /\ P2 /\ L))
    , ("&&" /\ ("and" /\ P3 /\ L))
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

neg :: Partial => Array Object -> Object
neg [ IntObj x ] = IntObj $ Prelude.negate x
neg [ FloatObj x ] = FloatObj $ Prelude.negate x

and :: Partial => Array Object -> Object
and [ BoolObj a, BoolObj b ] = BoolObj $ a && b

or :: Partial => Array Object -> Object
or [ BoolObj a, BoolObj b ] = BoolObj $ a || b

-- Number Fns
add :: Partial => Array Object -> Object
add [ IntObj a, IntObj b ] = IntObj $ a + b
add [ FloatObj a, FloatObj b ] = FloatObj $ a + b

sub :: Partial => Array Object -> Object
sub [ IntObj a, IntObj b ] = IntObj $ a - b
sub [ FloatObj a, FloatObj b ] = FloatObj $ a - b

mult :: Partial => Array Object -> Object
mult [ IntObj a, IntObj b ] = IntObj $ a * b
mult [ FloatObj a, FloatObj b ] = FloatObj $ a * b

div :: Partial => Array Object -> Object
div [ IntObj a, IntObj b ] = IntObj $ a / b
div [ FloatObj a, FloatObj b ] = FloatObj $ a / b

mod :: Partial => Array Object -> Object
mod [ IntObj a, IntObj b ] = IntObj $ Ring.mod a b

gcd :: Partial => Array Object -> Object
gcd [ IntObj a, IntObj b ] = IntObj $ Ring.gcd a b

lcm :: Partial => Array Object -> Object
lcm [ IntObj a, IntObj b ] = IntObj $ Ring.lcm a b

sum :: Partial => Array Object -> Object
sum [ ListObj xs ] = foldl1 (add <.. arr2) $ NonEmptyArray xs

product :: Partial => Array Object -> Object
product [ ListObj xs ] = foldl1 (mult <.. arr2) $ NonEmptyArray xs

-- List / String Fns
append :: Partial => Array Object -> Object
append [ StringObj a, StringObj b ] = StringObj $ a <> b
append [ ListObj a, ListObj b ] = ListObj $ a <> b

cons :: Partial => Array Object -> Object
cons [ CharObj a, StringObj b ] = StringObj $ String.singleton a <> b
cons [ a, ListObj b ] = ListObj $ Array.cons a b

snoc :: Partial => Array Object -> Object
snoc [ StringObj a, CharObj b ] = StringObj $ a <> String.singleton b
snoc [ ListObj a, b ] = ListObj $ Array.snoc a b

concat :: Partial => Array Object -> Object
concat [ ListObj xs ] = foldl1 (append <.. arr2) $ NonEmptyArray xs

arr2 :: forall a. a -> a -> Array a
arr2 a b = [ a, b ]

