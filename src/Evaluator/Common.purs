module App.Evaluator.Common where

import FatPrelude

import App.Components.Table.Cell (Cell, CellValue(..))
import App.Evaluator.Builtins as Builtins
import App.Evaluator.Errors (EvalError(..), LexicalError(..))
import App.Parser.Common (var)
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..), Var(..), VarOp(..), preludeModule)
import App.SyntaxTree.FnDef (Associativity(..), BuiltinFnInfo, FnBody(..), FnDef(..), FnInfo(..), Object(..), OpInfo, Precedence(..), Scope(..))
import App.SyntaxTree.Pattern (Pattern(..))
import Bookhound.FatPrelude (findJust)
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (is)
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.String (stripSuffix)
import Data.String as String
import Data.Tree.Zipper (Loc, fromTree, toTree)

type LocalFormulaCtx =
  { tableData :: Map Cell CellValue
  , fnsMap :: Map QVar FnInfo
  , operatorsMap :: Map QVarOp OpInfo
  , aliasedModulesMap :: Map (Module /\ Module) (Set Module)
  , importedModulesMap :: Map Module (Set Module)
  , localFnsMap :: Map (Scope /\ Var) FnInfo
  , argsMap :: Map (Scope /\ Var) FnInfo
  , module' :: Module
  , scope :: Scope
  , scopeLoc :: Loc Scope
  , lambdaCount :: Int
  }

type EvalM a = forall m. MonadState LocalFormulaCtx m => ExceptT EvalError m a

registerBindings :: Array FnDef -> EvalM Unit
registerBindings bindings = do
  { scope, scopeLoc } <- get
  let (Scope maxScope) = fromMaybe scope $ maximum $ toTree scopeLoc
  let scopes = Scope <<< (_ + maxScope) <$> (1 .. length bindings)
  traverse_ (\(n /\ x) -> registerLocalFn n x) (scopes `zip'` bindings)
  modify_ \st -> st
    { scopeLoc = appendChildren (mkLeaf <$> List.fromFoldable scopes)
        st.scopeLoc
    }

registerLocalFn :: Scope -> FnDef -> EvalM Unit
registerLocalFn scope fnDef =
  modify_ \st -> st { localFnsMap = insertFnDef scope fnDef st.localFnsMap }

registerArg :: Scope -> FnDef -> EvalM Unit
registerArg scope fnDef =
  modify_ \st -> st { argsMap = insertFnDef scope fnDef st.argsMap }

lookupFn :: QVar -> EvalM FnInfo
lookupFn (QVar fnModule (Var fnName))
  | Just newFn <- Var <$> stripSuffix (String.Pattern "Flipped") fnName =
      do
        FnInfo fnInfo <- lookupFn $ QVar fnModule newFn
        pure $ FnInfo $ fnInfo
          { body = FnApply (varFn "flip")
              [ Object' $ FnObj $ FnInfo fnInfo ]
          , params = []
          }
lookupFn qVar = case qVar of
  QVar Nothing var -> lookupLocalFn var <|> lookupModuleFn qVar
  QVar _ _ -> lookupModuleFn qVar

-- Scope resolution:
-- 1. Children bindings (Enclosing bindings)
-- 2. Fn args (Enclosing args)
-- 3. Siblings bindings (From the same where expression)
-- 4. Free variables (Closed over bindings + args)
lookupLocalFn :: Var -> EvalM FnInfo
lookupLocalFn fnName = do
  { localFnsMap, argsMap, scope, scopeLoc } <- get
  let
    lookupVar n = Map.lookup (n /\ fnName) localFnsMap
    lookupArg n = Map.lookup (n /\ fnName) argsMap
    lookupVarOrArg n = Map.lookup (n /\ fnName) (Map.union localFnsMap argsMap)
    childrenLookup = lookupVar <$> childrenValues scope scopeLoc
    siblingsLookup = lookupVar <$> siblingsValues scope scopeLoc
    argsLookup = lookupArg <$> nodeValues scope scopeLoc
    freeVarsLookup = lookupVarOrArg <$> ancestorsValues scope scopeLoc
  except
    $ note (LexicalError' $ UnknownValue $ QVar Nothing fnName)
    $ findJust
        (childrenLookup <> argsLookup <> siblingsLookup <> freeVarsLookup)

lookupModuleFn :: QVar -> EvalM FnInfo
lookupModuleFn qVar@(QVar fnModule fnName) = do
  st <- get
  let fns = getAvailableFns QVar (fnModule /\ fnName) st
  except
    $ note (LexicalError' $ UnknownValue qVar)
    $ findJust
    $ flip Map.lookup st.fnsMap
    <$> fns

lookupOperator :: QVarOp -> EvalM OpInfo
lookupOperator (QVarOp opModule opName@(VarOp op))
  | Right (Var fnName) <- runParser (is "|" *> var <* is ">") op = pure
      { id: { opModule: preludeModule, opName }
      , fnName: QVar opModule $ Var (fnName <> "Flipped")
      , precedence: P1
      , associativity: L
      }
  | Right fnName <- runParser (is "<" *> var <* is "|") op = pure
      { id: { opModule: preludeModule, opName }
      , fnName: QVar opModule fnName
      , precedence: P0
      , associativity: R
      }

lookupOperator qVarOp@(QVarOp opModule opName) = do
  st <- get
  let ops = getAvailableFns QVarOp (opModule /\ opName) st
  except
    $ note (LexicalError' $ UnknownOperator qVarOp)
    $ findJust
    $ flip Map.lookup st.operatorsMap
    <$> ops

lookupBuiltinFn :: Var -> EvalM BuiltinFnInfo
lookupBuiltinFn fnName =
  except
    $ note (LexicalError' $ UnknownValue $ QVar Nothing fnName)
    $ Map.lookup fnName Builtins.builtinFnsMap

getAvailableFns
  :: forall a b
   . (Maybe Module -> a -> b)
  -> (Maybe Module /\ a)
  -> LocalFormulaCtx
  -> NonEmptyArray b
getAvailableFns
  ctor
  (fnModule /\ fnName)
  { module', importedModulesMap, aliasedModulesMap } =
  flip ctor fnName <<< pure <$> cons' module' (Array.fromFoldable modules)
  where
  modules = case fnModule of
    Just alias -> fromMaybe Set.empty $ Map.lookup (module' /\ alias)
      aliasedModulesMap
    Nothing -> fromMaybe Set.empty $ Map.lookup
      module'
      importedModulesMap

insertFnDef
  :: Scope -> FnDef -> Map (Scope /\ Var) FnInfo -> Map (Scope /\ Var) FnInfo
insertFnDef scope (FnDef fnName params returnType body) =
  Map.insert (scope /\ fnName) fnInfo
  where
  fnInfo = FnInfo
    { id: Nothing, params, body, scope, argsMap: Map.empty, returnType }

getNewFnState :: FnInfo -> Array FnBody -> EvalM LocalFormulaCtx
getNewFnState (FnInfo { id: maybeFnId, scope, params, argsMap }) fnArgs =
  do
    st <- get
    let newArgsMap = Map.union argsMap $ Map.union argBindings st.argsMap
    pure $ case maybeFnId of
      Just { fnModule } ->
        st
          { argsMap = newArgsMap
          , localFnsMap = Map.empty
          , module' = fnModule
          , scope = zero
          , scopeLoc = fromTree $ mkLeaf zero
          }
      Nothing -> st
        { argsMap = newArgsMap
        , scope = scope
        , scopeLoc = goToNode scope st.scopeLoc
        }
  where
  argBindings = Map.fromFoldable
    $ rmap
        ( FnInfo <<<
            { id: Nothing
            , body: _
            , scope
            , params: []
            , argsMap: Map.empty
            , returnType: Nothing
            }
        )
    <$> zip' ((scope /\ _) <<< fst <$> params) args
  args =
    if isJust maybeFnId then resetScope <$> fnArgs
    else fnArgs

substituteFnArgs :: Object -> Array (Var /\ FnBody) -> Object
substituteFnArgs (FnObj (FnInfo fn)) pairs =
  FnObj $ FnInfo $ fn { body = foldl substituteArg fn.body pairs }
substituteFnArgs x _ = x

resetFnScope :: Object -> Object
resetFnScope (FnObj (FnInfo fn)) =
  FnObj $ FnInfo $ fn { scope = zero }
resetFnScope x = x

substituteArg :: FnBody -> (Var /\ FnBody) -> FnBody
substituteArg (FnApply f xs) pair =
  FnApply (substituteArg f pair)
    (flip substituteArg pair <$> xs)

substituteArg fnVar@(FnVar x) (param /\ arg)
  | x == QVar Nothing param = arg
  | otherwise = fnVar

substituteArg (InfixFnApply ops bodies) pair =
  InfixFnApply ops (flip substituteArg pair <$> bodies)

substituteArg body _ = body

resetScope :: FnBody -> FnBody
resetScope (Object' (FnObj (FnInfo fnInfo))) =
  Object' $ FnObj $ FnInfo fnInfo { scope = zero }

resetScope x = x

isSpread :: Pattern -> Boolean
isSpread Spread = true
isSpread (AliasedPattern _ Spread) = true
isSpread _ = false

extractAlias :: Pattern -> Maybe Var
extractAlias (AliasedPattern alias _) = Just alias
extractAlias _ = Nothing

nonEmptyCellValue :: CellValue -> Boolean
nonEmptyCellValue (StringVal "") = false
nonEmptyCellValue _ = true

varFn :: String -> FnBody
varFn = FnVar <<< QVar Nothing <<< Var

lambdaId :: Int -> String
lambdaId n = "__lambda__" <> show n
