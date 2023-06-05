module App.SyntaxTree.Pattern where

import FatPrelude

import App.SyntaxTree.Common (Literal, QCtor, QCtorOp, Var)

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
  | WildcardRecordPattern
      { ctor :: QCtor
      , namedFields :: Array (Var /\ Maybe Pattern)
      }
  | AliasedPattern Var Pattern
  | TypeAnnotation Pattern Type
  | ListPattern (Array Pattern)
  | TuplePattern (Array Pattern)
  | VarPattern Var
  | LitPattern Literal
  | Wildcard
