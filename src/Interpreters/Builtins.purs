module App.Interpreters.Builtins
  ( builtins1
  , builtins2
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
import FatPrelude (Map, Set, bimap, lmap, ($), (&&), (*), (+), (-), (/), (/\), (<$>), (<>), (||))
import Prelude as Prelude

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

builtins1 :: Partial => Map Var (Literal -> Literal)
builtins1 = Map.fromFoldable $ lmap Var <$>
  [ ("not" /\ not)
  , ("neg" /\ neg)
  , ("sum" /\ sum)
  , ("product" /\ product)
  ]

builtins2 :: Partial => Map Var (Literal -> Literal -> Literal)
builtins2 = Map.fromFoldable $ lmap Var <$>
  [ ("and" /\ and)
  , ("or" /\ or)
  , ("add" /\ add)
  , ("sub" /\ sub)
  , ("mult" /\ mult)
  , ("div" /\ div)
  , ("mod" /\ mod)
  , ("gcd" /\ gcd)
  , ("lcm" /\ lcm)
  , ("append" /\ append)
  , ("cons" /\ cons)
  , ("snoc" /\ snoc)
  ]

-- Unary functions
not :: Partial => Literal -> Literal
not (BoolLit x) = BoolLit (Prelude.not x)

neg :: Partial => Literal -> Literal
neg (IntLit x) = IntLit (Prelude.negate x)
neg (FloatLit x) = FloatLit (Prelude.negate x)

sum :: Partial => Literal -> Literal
sum (ListLit []) = IntLit 0
sum (ListLit xs) = foldl1 add $ NonEmptyArray xs

product :: Partial => Literal -> Literal
product (ListLit []) = IntLit 1
product (ListLit xs) = foldl1 mult $ NonEmptyArray xs

-- Binary functions
and :: Partial => Literal -> Literal -> Literal
and (BoolLit a) (BoolLit b) = BoolLit $ a && b

or :: Partial => Literal -> Literal -> Literal
or (BoolLit a) (BoolLit b) = BoolLit $ a || b

add :: Partial => Literal -> Literal -> Literal
add (IntLit a) (IntLit b) = IntLit $ a + b
add (FloatLit a) (FloatLit b) = FloatLit $ a + b

sub :: Partial => Literal -> Literal -> Literal
sub (IntLit a) (IntLit b) = IntLit $ a - b
sub (FloatLit a) (FloatLit b) = FloatLit $ a - b

mult :: Partial => Literal -> Literal -> Literal
mult (IntLit a) (IntLit b) = IntLit $ a * b
mult (FloatLit a) (FloatLit b) = FloatLit $ a * b

div :: Partial => Literal -> Literal -> Literal
div (IntLit a) (IntLit b) = IntLit $ a / b
div (FloatLit a) (FloatLit b) = FloatLit $ a / b

mod :: Partial => Literal -> Literal -> Literal
mod (IntLit a) (IntLit b) = IntLit $ Ring.mod a b

gcd :: Partial => Literal -> Literal -> Literal
gcd (IntLit a) (IntLit b) = IntLit $ Ring.gcd a b

lcm :: Partial => Literal -> Literal -> Literal
lcm (IntLit a) (IntLit b) = IntLit $ Ring.lcm a b

append :: Partial => Literal -> Literal -> Literal
append (StringLit a) (StringLit b) = StringLit $ a <> b
append (ListLit a) (ListLit b) = ListLit $ a <> b

cons :: Partial => Literal -> Literal -> Literal
cons (CharLit a) (StringLit b) = StringLit $ String.singleton a <> b
cons a (ListLit b) = ListLit $ Array.cons a b

snoc :: Partial => Literal -> Literal -> Literal
snoc (StringLit a) (CharLit b) = StringLit $ a <> String.singleton b
snoc (ListLit a) b = ListLit $ Array.snoc a b
