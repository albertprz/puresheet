module App.SyntaxTrees.FnDef where

import FatPrelude

import App.SyntaxTrees.Common (Literal, QCtor, QCtorOp, QVar, QVarOp, Var)
import App.SyntaxTrees.Pattern (Pattern)

data FnDef = FnDef (Array Var) (Array Pattern) MaybeGuardedFnBody

data FnBody
  = FnApply FnBody (Array FnBody)
  | InfixFnApply (Array FnOp) (Array FnBody)
  | LeftOpSection FnOp FnBody
  | RightOpSection FnBody FnOp
  | Bindings FnBody (Array FnDef)
  | MultiWayIfExpr (Array GuardedFnBody)
  | DoExpr (Array DoStep)
  | SwitchExpr FnBody (Array CaseBinding)
  | RecordCreate QCtor (Array (Var /\ FnBody))
  | RecordUpdate FnBody (Array (Var /\ FnBody))
  | ListRange FnBody (Maybe FnBody)
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

data GuardedFnBody = GuardedFnBody Guard FnBody

data Guard
  = Guard (Array PatternGuard)
  | Otherwise

data PatternGuard
  = PatternGuard Pattern FnBody
  | SimpleGuard FnBody

