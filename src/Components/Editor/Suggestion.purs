module App.Editor.Suggestion where

import FatPrelude hiding (div)

import App.Evaluator.Builtins as Builtins
import App.Evaluator.Common (LocalFormulaCtx, getAvailableAliases, getAvailableModules, lookupBuiltinFn, lookupModuleFn, lookupOperator)
import App.Parser.Common (ident, module', operator, qTerm, qVar, qVarOp)
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..), Var(..), VarOp(..), preludeModule)
import App.SyntaxTree.FnDef (FnSig)
import App.Utils.SyntaxAtom (SyntaxAtom, condenseSyntaxAtoms, fnSigToSyntaxAtoms, syntaxAtomParser, syntaxAtomToClassName)
import Bookhound.Parser (Parser, runParser)
import Bookhound.ParserCombinators (is)
import Bookhound.Parsers.Char (lower)
import Data.Array as Array
import Data.HashMap as HashMap
import Data.Set as Set
import Data.String.CodeUnits (indexOf', lastIndexOf')
import Data.String.CodeUnits (length, slice) as String
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (endsWith, startsWith) as String
import Halogen.HTML (HTML, span, text)
import Halogen.HTML.Properties (class_)
import Record.Extra (pick)

formulaElements :: forall a b. String -> Array (HTML a b)
formulaElements =
  syntaxAtomsToElements <<< fold <<< runParser syntaxAtomParser

fnSigElements :: forall a b. QVar -> FnSig -> Array (HTML a b)
fnSigElements =
  syntaxAtomsToElements <.. fnSigToSyntaxAtoms

syntaxAtomsToElements :: forall a b. Array SyntaxAtom -> Array (HTML a b)
syntaxAtomsToElements = map toElement <<< condenseSyntaxAtoms
  where
  toElement atom = span
    [ class_ $ syntaxAtomToClassName atom ]
    [ text $ show atom ]

getFnAtIndex :: String -> Int -> Maybe SuggestionTerm
getFnAtIndex formulaText idx =
  hush $ runParser fnParser currentWord
  where
  fnParser = OpSuggestion <$> qVarOp <|> FnSuggestion <$> qVar
  { currentWord } = getWordAtIndex [] formulaText idx

getSuggestionsAtIndex
  :: LocalFormulaCtx -> String -> Int -> Array SuggestionTerm
getSuggestionsAtIndex ctx formulaText idx =
  Array.take (inc $ unwrap $ top @SuggestionId)
    $ filter (not <<< uniqueSuggestionPred)
    $ Array.fromFoldable vars
  where
  { currentWord, endIndex } = getWordAtIndex [] formulaText idx
  vars = whenMonoid (idx == endIndex)
    $ fold
    $ hush
    $ runParser varParser currentWord
  varParser =
    ((_ `filterFns` ctx) <$> partialQVar)
      <|> ((_ `filterOps` ctx) <$> partialQVarOp)
      <|> ((_ `getAllAvailableFns` ctx) <<< Just <$> (module' <* is '.'))
      <|> ((_ `filterModules` ctx) <$> module')
  uniqueSuggestionPred x =
    String.endsWith (show x) currentWord && Set.size vars == 1

partialQVar :: Parser QVar
partialQVar = uncurry QVar <$> qTerm (Var <$> ident lower)

partialQVarOp :: Parser QVarOp
partialQVarOp = uncurry QVarOp <$> qTerm (VarOp <$> operator)

getWordAtIndex
  :: Array String
  -> String
  -> Int
  -> { currentWord :: String
     , endIndex :: Int
     , startIndex :: Int
     }
getWordAtIndex otherSeparators formulaText idx =
  { currentWord: String.slice startIndex endIndex formulaText
  , startIndex
  , endIndex
  }
  where
  startIndex = fromMaybe 0 $ map inc
    $ maximum
    $ filterMap (\pat -> lastIndexOf' pat idx formulaText)
    $ map Pattern ([ "(", "[" ] <> separators)
  endIndex = fromMaybe (String.length formulaText)
    $ minimum
    $ filterMap (\pat -> indexOf' pat idx formulaText)
    $ map Pattern ([ ")", "]" ] <> separators)
  separators = [ " ", " ", "\n", "\t", "," ] <> otherSeparators

getFnSig :: QVar -> LocalFormulaCtx -> Maybe FnSig
getFnSig qVar ctx =
  pick <$> builtinFnInfo <|> pick <$> fnInfo
  where
  builtinFnInfo =
    hush
      $ flip evalState ctx
      $ runExceptT
      $ lookupBuiltinFn qVar
  fnInfo =
    map unwrap
      $ hush
      $ flip evalState ctx
      $ runExceptT
      $ lookupModuleFn qVar

filterFns :: QVar -> LocalFormulaCtx -> Set SuggestionTerm
filterFns (QVar fnModule fnVar) ctx = moduleFns <> builtinFns
  where
  modules = getAvailableModules fnModule ctx
  moduleFns =
    Set.map FnSuggestion
      $ Set.fromFoldable
      $ filter filterFn
      $ HashMap.keys ctx.fnsMap
  builtinFns =
    Set.map BuiltinFnSuggestion
      $ Set.fromFoldable
      $ filter filterFn
      $ HashMap.keys Builtins.builtinFnsMap
  filterFn (QVar module' fn) =
    all (flip Set.member modules) module'
      && String.startsWith (unwrap fnVar) (unwrap fn)

filterOps :: QVarOp -> LocalFormulaCtx -> Set SuggestionTerm
filterOps (QVarOp opModule opVar) ctx =
  Set.fromFoldable
    $ map OpSuggestion
    $ filter filterOp
    $ HashMap.keys ctx.operatorsMap
  where
  modules = getAvailableModules opModule ctx
  filterOp (QVarOp module' op) =
    all (flip Set.member modules) module'
      && String.startsWith (unwrap opVar) (unwrap op)

filterModules :: Module -> LocalFormulaCtx -> Set SuggestionTerm
filterModules fnModule ctx =
  Set.map ModuleSuggestion
    $ Set.filter filterModule (aliases <> modules <> ctx.modules)
  where
  modules = getAvailableModules Nothing ctx
  aliases = getAvailableAliases ctx
  filterModule module' =
    String.startsWith (fold $ unwrap fnModule) (fold $ unwrap module')

getAllAvailableFns :: Maybe Module -> LocalFormulaCtx -> Set SuggestionTerm
getAllAvailableFns fnModule ctx =
  fns <> builtins <> ops
  where
  modules = case fnModule of
    Just _ -> getAvailableModules fnModule ctx
    Nothing -> ctx.modules
  fns = Set.fromFoldable
    $ map FnSuggestion
    $ filter filterFn
    $ HashMap.keys ctx.fnsMap
  builtins = Set.fromFoldable
    $ map BuiltinFnSuggestion
    $ filter filterFn
    $ HashMap.keys Builtins.builtinFnsMap
  ops = Set.fromFoldable
    $ map OpSuggestion
    $ filter filterOp
    $ HashMap.keys ctx.operatorsMap
  filterFn (QVar module' _) =
    all (flip Set.member modules) module'
  filterOp (QVarOp module' _) =
    all (flip Set.member modules) module'

extractSuggestionFn :: LocalFormulaCtx -> SuggestionTerm -> Maybe QVar
extractSuggestionFn ctx = case _ of
  FnSuggestion var -> Just var
  BuiltinFnSuggestion var -> Just var
  OpSuggestion op -> _.fnName
    <$> hush (flip evalState ctx $ runExceptT $ lookupOperator op)
  ModuleSuggestion _ -> Nothing

moduleFromSuggestion :: SuggestionTerm -> Maybe Module
moduleFromSuggestion = case _ of
  FnSuggestion (QVar x _) -> x
  OpSuggestion (QVarOp x _) -> x
  BuiltinFnSuggestion (QVar x _) -> x
  ModuleSuggestion x -> Just x

data SuggestionTerm
  = FnSuggestion QVar
  | BuiltinFnSuggestion QVar
  | OpSuggestion QVarOp
  | ModuleSuggestion Module

newtype SuggestionId = SuggestionId Int

derive instance Eq SuggestionTerm

instance Ord SuggestionTerm where
  compare (FnSuggestion _) (OpSuggestion _) = LT
  compare (BuiltinFnSuggestion _) (OpSuggestion _) = LT
  compare (OpSuggestion _) (FnSuggestion _) = GT
  compare (OpSuggestion _) (BuiltinFnSuggestion _) = GT
  compare (OpSuggestion x) (OpSuggestion y)
    | String.length (show x) /= String.length (show y) =
        comparing (String.length <<< show) x y
  compare x y
    | show x == show y =
        if moduleFromSuggestion x == Just preludeModule then LT
        else if moduleFromSuggestion y == Just preludeModule then GT
        else comparing (show <<< moduleFromSuggestion) x y
  compare x y = comparing show x y

instance Show SuggestionTerm where
  show = case _ of
    FnSuggestion (QVar _ fn) -> show fn
    OpSuggestion (QVarOp _ op) -> show op
    BuiltinFnSuggestion (QVar _ fn) -> show fn
    ModuleSuggestion module' -> show module'

derive instance Newtype SuggestionId _
derive newtype instance Eq SuggestionId
derive newtype instance Ord SuggestionId
derive newtype instance Semiring SuggestionId
derive newtype instance Ring SuggestionId

instance Bounded SuggestionId where
  bottom = wrap 0
  top = wrap 9
