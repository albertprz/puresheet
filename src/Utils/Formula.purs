module App.Utils.Formula where

import FatPrelude hiding (div)

import App.CSS.ClassNames (materialIcons, suggestionOption)
import App.Components.Table.SyntaxAtom (SyntaxAtom, condenseSyntaxAtoms, fnSigToSyntaxAtoms, syntaxAtomParser, syntaxAtomToClassName)
import App.Evaluator.Builtins as Builtins
import App.Evaluator.Common (LocalFormulaCtx, getAvailableAliases, getAvailableModules, lookupBuiltinFn, lookupModuleFn, lookupOperator)
import App.Parser.Common (ident, module', operator, qTerm, qVar, qVarOp)
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..), Var(..), VarOp(..))
import App.SyntaxTree.FnDef (SimpleFnSig)
import App.Utils.Monoid (whenPlus)
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (is)
import Bookhound.Parsers.Char (lower)
import Data.Array as Array
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf', lastIndexOf')
import Data.String.CodeUnits (length, slice) as String
import Data.String.Utils (startsWith) as String
import Halogen.HTML (HTML, span, text)
import Halogen.HTML.Elements (div, i)
import Halogen.HTML.Properties (class_)
import Record.Extra (pick)

formulaElements :: forall a b. String -> Array (HTML a b)
formulaElements =
  syntaxAtomsElements <<< fold <<< runParser syntaxAtomParser

fnSigElements :: forall a b. QVar -> SimpleFnSig -> Array (HTML a b)
fnSigElements =
  syntaxAtomsElements <.. fnSigToSyntaxAtoms

syntaxAtomsElements :: forall a b. Array SyntaxAtom -> Array (HTML a b)
syntaxAtomsElements = map toElement <<< condenseSyntaxAtoms
  where
  toElement atom = span
    [ class_ $ syntaxAtomToClassName atom ]
    [ text $ show atom ]

suggestionsElements :: forall a b. Array SuggestionTerm -> Array (HTML a b)
suggestionsElements suggestions =
  map toElement
    $ Array.sort
    $ Array.take 10
    $ map show suggestions
  where
  toElement fnName = div
    [ class_ suggestionOption ]
    [ i
        [ class_ materialIcons ]
        [ text "functions" ]
    , text fnName
    ]

getFnAtIndex :: LocalFormulaCtx -> String -> Int -> Maybe QVar
getFnAtIndex ctx formulaText idx =
  join (hush $ runParser fnParser currentWord)
  where
  fnParser = lookupOp <$> qVarOp <|> Just <$> qVar
  lookupOp op = _.fnName
    <$> hush (flip evalState ctx $ runExceptT $ lookupOperator op)
  { currentWord } = getWordAtIndex formulaText idx

getSuggestionsAtIndex
  :: LocalFormulaCtx -> String -> Int -> Array SuggestionTerm
getSuggestionsAtIndex ctx formulaText idx =
  Array.nub
    $ fold
    $ whenPlus (idx == endIndex)
    $ hush
    $ runParser varParser currentWord
  where
  partialQVarOp = uncurry QVarOp <$> qTerm (VarOp <$> operator)
  partialQVar = uncurry QVar <$> qTerm (Var <$> ident lower)
  varParser =
    ((_ `filterFns` ctx) <$> partialQVar)
      <|> ((_ `filterOps` ctx) <$> partialQVarOp)
      <|> ((_ `getAllAvailableFns` ctx) <$> (module' <* is '.'))
      <|> ((_ `filterModules` ctx) <$> module')
  { currentWord, endIndex } = getWordAtIndex formulaText idx

getWordAtIndex
  :: String
  -> Int
  -> { currentWord :: String
     , endIndex :: Int
     , startIndex :: Int
     }
getWordAtIndex formulaText idx =
  { currentWord: String.slice startIndex endIndex formulaText
  , startIndex
  , endIndex
  }
  where
  startIndex = fromMaybe 0 $ map inc
    $ maximum
    $ filterMap (\pat -> lastIndexOf' pat idx formulaText)
    $ map Pattern (separators <> [ "(", "[" ])
  endIndex = fromMaybe (String.length formulaText)
    $ minimum
    $ filterMap (\pat -> indexOf' pat idx formulaText)
    $ map Pattern (separators <> [ ")", "]" ])
  separators = [ " ", " ", "\n", "\t", "," ]

getFnSig :: QVar -> LocalFormulaCtx -> Maybe SimpleFnSig
getFnSig qVar@(QVar fnModule fnName) ctx =
  pick <$> builtinFnInfo <|> pick <$> fnInfo
  where
  builtinFnInfo =
    whenPlus (isNothing fnModule)
      $ hush
      $ flip evalState ctx
      $ runExceptT
      $ lookupBuiltinFn fnName
  fnInfo =
    map unwrap
      $ hush
      $ flip evalState ctx
      $ runExceptT
      $ lookupModuleFn qVar

filterFns :: QVar -> LocalFormulaCtx -> Array SuggestionTerm
filterFns (QVar fnModule fnVar) st =
  map VarSuggestion (builtinFns <> moduleFns)
  where
  modules = HashSet.fromFoldable $ getAvailableModules fnModule st
  moduleFns =
    filter filterFn
      $ HashMap.keys st.fnsMap
  builtinFns = whenMonoid (isNothing fnModule)
    ( map (QVar Nothing)
        $ filter filterBuiltinFn
        $ HashMap.keys Builtins.builtinFnsMap
    )
  filterFn (QVar module' fn) =
    all (flip HashSet.member modules) module'
      && String.startsWith (unwrap fnVar) (unwrap fn)
  filterBuiltinFn fn =
    String.startsWith (unwrap fnVar) (unwrap fn)

filterOps :: QVarOp -> LocalFormulaCtx -> Array SuggestionTerm
filterOps (QVarOp opModule opVar) st =
  map VarOpSuggestion
    $ filter filterOp
    $ HashMap.keys st.operatorsMap
  where
  modules = HashSet.fromFoldable $ getAvailableModules opModule st
  filterOp (QVarOp module' op) =
    all (flip HashSet.member modules) module'
      && String.startsWith (unwrap opVar) (unwrap op)

filterModules :: Module -> LocalFormulaCtx -> Array SuggestionTerm
filterModules fnModule st =
  map ModuleSuggestion
    $ filter filterModule (aliases <> modules)
  where
  modules = getAvailableModules Nothing st
  aliases = getAvailableAliases st
  filterModule module' =
    String.startsWith (fold $ unwrap fnModule) (fold $ unwrap module')

getAllAvailableFns
  :: Module -> LocalFormulaCtx -> Array SuggestionTerm
getAllAvailableFns fnModule st =
  fns <> ops
  where
  modules = HashSet.fromFoldable $ getAvailableModules (Just fnModule) st
  fns = map VarSuggestion
    $ filter filterFn
    $ HashMap.keys st.fnsMap
  ops = map VarOpSuggestion
    $ filter filterOp
    $ HashMap.keys st.operatorsMap
  filterFn (QVar module' _) =
    all (flip HashSet.member modules) module'
  filterOp (QVarOp module' _) =
    all (flip HashSet.member modules) module'

data SuggestionTerm
  = ModuleSuggestion Module
  | VarOpSuggestion QVarOp
  | VarSuggestion QVar

instance Show SuggestionTerm where
  show = case _ of
    VarOpSuggestion (QVarOp _ op) -> show op
    VarSuggestion (QVar _ fn) -> show fn
    ModuleSuggestion module' -> show module'

derive instance Eq SuggestionTerm
derive instance Ord SuggestionTerm
