module App.SyntaxTrees.Pattern where

import FatPrelude

import App.SyntaxTrees.Common (Literal, QCtor, QCtorOp, Var)

data Pattern
  = CtorPattern
      { ctor :: QCtor
      , fields :: Array Pattern
      }
  | InfixCtorPattern
      { ctorOp :: QCtorOp
      , fields :: Array Pattern
      }
  | RecordPattern
      { ctor :: QCtor
      , namedFields :: Array (Var /\ Maybe Pattern)
      }
  | AliasedPattern Var Pattern
  | ListPattern (Array Pattern)
  | TuplePattern (Array Pattern)
  | VarPattern Var
  | LitPattern Literal
  | Wildcard

ctorPattern :: QCtor -> Array Pattern -> Pattern
ctorPattern ctor fields = CtorPattern { ctor, fields }

infixCtorPattern :: QCtorOp -> Array Pattern -> Pattern
infixCtorPattern ctorOp fields = InfixCtorPattern { ctorOp, fields }

recordPattern :: QCtor -> Array (Var /\ Maybe Pattern) -> Pattern
recordPattern ctor namedFields = RecordPattern { ctor, namedFields }
