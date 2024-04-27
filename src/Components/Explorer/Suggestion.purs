module App.Explorer.Suggestion where

import FatPrelude

import App.Editor.Suggestion (SuggestionTerm, extractSuggestionFn, getFnSig)
import App.Evaluator.Common (LocalFormulaCtx)
import App.SyntaxTree.Common (QVar)
import App.SyntaxTree.FnDef (FnSig)

newtype SuggestionInfo = SuggestionInfo
  { term :: SuggestionTerm, fn :: QVar, fnSig :: FnSig }

derive instance Newtype SuggestionInfo _

instance Eq SuggestionInfo where
  eq = eq `on` (_.term <<< unwrap)

instance Ord SuggestionInfo where
  compare = compare `on` (_.term <<< unwrap)

instance Show SuggestionInfo where
  show = show <<< _.term <<< unwrap

getSuggestionInfo :: LocalFormulaCtx -> SuggestionTerm -> Maybe SuggestionInfo
getSuggestionInfo ctx term = do
  fn <- extractSuggestionFn ctx term
  fnSig <- getFnSig fn ctx
  pure $ SuggestionInfo { term, fn, fnSig }
