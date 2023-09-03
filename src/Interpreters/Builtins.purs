module App.Interpreters.Builtins (builtins, operatorsMap, priorityMap) where

import App.SyntaxTrees.Common (Literal(..), Var(..), VarOp(..))
import Data.EuclideanRing as Ring
import Data.Map as Map
import FatPrelude (Map, bimap, lmap, ($), (*), (+), (-), (/), (/\), (<$>), (<>))

operatorsMap :: Map VarOp Var
operatorsMap = Map.fromFoldable $ bimap VarOp Var <$>
  [ ("+" /\ "add")
  , ("-" /\ "sub")
  , ("*" /\ "mult")
  , ("/" /\ "div")
  , ("%" /\ "mod")
  ]

priorityMap :: Map VarOp Int
priorityMap = Map.fromFoldable $ lmap VarOp <$>
  [ ("+" /\ 6)
  , ("-" /\ 7)
  , ("*" /\ 8)
  , ("/" /\ 9)
  , ("%" /\ 10)
  ]

builtins :: Partial => Map Var (Literal -> Literal -> Literal)
builtins = Map.fromFoldable $ lmap Var <$>
  [ ("add" /\ add)
  , ("sub" /\ sub)
  , ("mult" /\ mult)
  , ("div" /\ div)
  , ("mod" /\ mod)
  , ("gcd" /\ gcd)
  , ("lcm" /\ lcm)
  , ("append" /\ append)
  ]

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
