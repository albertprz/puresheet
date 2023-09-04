module App.Interpreters.Builtins
  ( fnsMap
  , operatorsMap
  , precedenceMap
  , rAssocSet
  ) where

import App.SyntaxTrees.Common (Literal(..), Var(..), VarOp(..))
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.EuclideanRing as Ring
import Data.Map as Map
import Data.Semigroup.Foldable (foldl1)
import Data.Set as Set
import Data.String.CodeUnits as String
import FatPrelude (type (/\), Map, Set, bimap, lmap, ($), (&&), (*), (+), (-), (/), (/\), (<$>), (<..), (<>), (||))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

fnsMap :: Map Var ((Array Literal -> Literal) /\ Int)
fnsMap = unsafePartial $ Map.fromFoldable $ lmap Var <$>
  [ ("not" /\ (not /\ 1))
  , ("neg" /\ (neg /\ 1))
  , ("sum" /\ (sum /\ 2))
  , ("product" /\ (product /\ 2))
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

operatorsMap :: Map VarOp Var
operatorsMap = Map.fromFoldable $ bimap VarOp Var <$>
  [ ("&&" /\ "and")
  , ("||" /\ "or")
  , ("+" /\ "add")
  , ("-" /\ "sub")
  , ("*" /\ "mult")
  , ("/" /\ "div")
  , ("%" /\ "mod")
  , ("++" /\ "append")
  , ("+:" /\ "cons")
  , (":+" /\ "snoc")
  ]

precedenceMap :: Map VarOp Int
precedenceMap = Map.fromFoldable $ lmap VarOp <$>
  [ ("||" /\ 2)
  , ("&&" /\ 3)
  , ("++" /\ 5)
  , ("+:" /\ 6)
  , (":+" /\ 7)
  , ("+" /\ 8)
  , ("-" /\ 9)
  , ("*" /\ 10)
  , ("/" /\ 11)
  , ("%" /\ 12)
  ]

rAssocSet :: Set VarOp
rAssocSet = Set.fromFoldable $ VarOp <$>
  [ "++", "+:" ]

not :: Partial => Array Literal -> Literal
not [ BoolLit x ] = BoolLit $ Prelude.not x

neg :: Partial => Array Literal -> Literal
neg [ IntLit x ] = IntLit $ Prelude.negate x
neg [ FloatLit x ] = FloatLit $ Prelude.negate x

and :: Partial => Array Literal -> Literal
and [ BoolLit a, BoolLit b ] = BoolLit $ a && b

or :: Partial => Array Literal -> Literal
or [ BoolLit a, BoolLit b ] = BoolLit $ a || b

add :: Partial => Array Literal -> Literal
add [ IntLit a, IntLit b ] = IntLit $ a + b
add [ FloatLit a, FloatLit b ] = FloatLit $ a + b

sub :: Partial => Array Literal -> Literal
sub [ IntLit a, IntLit b ] = IntLit $ a - b
sub [ FloatLit a, FloatLit b ] = FloatLit $ a - b

mult :: Partial => Array Literal -> Literal
mult [ IntLit a, IntLit b ] = IntLit $ a * b
mult [ FloatLit a, FloatLit b ] = FloatLit $ a * b

div :: Partial => Array Literal -> Literal
div [ IntLit a, IntLit b ] = IntLit $ a / b
div [ FloatLit a, FloatLit b ] = FloatLit $ a / b

mod :: Partial => Array Literal -> Literal
mod [ IntLit a, IntLit b ] = IntLit $ Ring.mod a b

gcd :: Partial => Array Literal -> Literal
gcd [ IntLit a, IntLit b ] = IntLit $ Ring.gcd a b

lcm :: Partial => Array Literal -> Literal
lcm [ IntLit a, IntLit b ] = IntLit $ Ring.lcm a b

sum :: Partial => Array Literal -> Literal
sum [ ListLit [] ] = IntLit 0
sum [ ListLit xs ] = foldl1 (add <.. arr2) $ NonEmptyArray xs

product :: Partial => Array Literal -> Literal
product [ ListLit [] ] = IntLit 1
product [ ListLit xs ] = foldl1 (mult <.. arr2) $ NonEmptyArray xs

append :: Partial => Array Literal -> Literal
append [ StringLit a, StringLit b ] = StringLit $ a <> b
append [ ListLit a, ListLit b ] = ListLit $ a <> b

cons :: Partial => Array Literal -> Literal
cons [ CharLit a, StringLit b ] = StringLit $ String.singleton a <> b
cons [ a, ListLit b ] = ListLit $ Array.cons a b

snoc :: Partial => Array Literal -> Literal
snoc [ StringLit a, CharLit b ] = StringLit $ a <> String.singleton b
snoc [ ListLit a, b ] = ListLit $ Array.snoc a b

arr2 :: forall t202. t202 -> t202 -> Array t202
arr2 a b = [ a, b ]
