module App.SyntaxTrees.FnDef where

import FatPrelude

import App.SyntaxTrees.Common (Literal, QCtor, QCtorOp, QVar, QVarOp, Var)
import App.SyntaxTrees.Pattern (Pattern)

data FnDef = FnDef
  { names :: Array Var
  , args :: Array Pattern
  , body :: MaybeGuardedFnBody
  }

data FnBody
  = FnApply
      { fn :: FnBody
      , args :: Array FnBody
      }
  | InfixFnApply
      { fnOps :: Array FnOp
      , args :: Array FnBody
      }
  | LeftOpSection
      { fnOp :: FnOp
      , arg :: FnBody
      }
  | RightOpSection
      { arg :: FnBody
      , fnOp :: FnOp
      }
  | LambdaExpr
      { patterns :: Array Pattern
      , body :: FnBody
      }
  | WhereExpr
      { body :: FnBody
      , fnBindings :: Array FnDef
      }
  | MultiWayIfExpr
      { whenExprs :: Array GuardedFnBody
      }
  | DoExpr
      { steps :: Array DoStep }
  | CaseOfExpr
      { matchee :: FnBody
      , cases :: Array CaseBinding
      }
  | LambdaCaseExpr
      { cases :: Array CaseBinding
      }
  | RecordCreate
      { ctor :: QCtor
      , namedFields :: Array (Var /\ FnBody)
      }
  | RecordUpdate
      { var :: FnBody
      , namedFields :: Array (Var /\ FnBody)
      }
  | ListRange Literal Literal
  | List (Array FnBody)
  | FnVar' FnVar
  | FnOp' FnOp
  | Literal' Literal

data FnVar
  = Selector Var
  | Selection QVar (Array Var)
  | Var' QVar
  | Ctor' QCtor

data FnOp
  = VarOp' QVarOp
  | CtorOp' QCtorOp

data DoStep
  = DoBinding (Array Var) FnBody
  | LetBinding FnDef
  | Body FnBody

data CaseBinding = CaseBinding Pattern MaybeGuardedFnBody

data MaybeGuardedFnBody
  = Guarded (Array GuardedFnBody)
  | Standard FnBody

data GuardedFnBody = GuardedFnBody
  { guard :: Guard
  , body :: FnBody
  }

data Guard
  = Guard (Array PatternGuard)
  | Otherwise

data PatternGuard
  = PatternGuard Pattern FnBody
  | SimpleGuard FnBody
