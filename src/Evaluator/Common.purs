module App.Evaluator.Common where

import FatPrelude

import App.Components.Spreadsheet.Cell (Cell, CellValue(..))
import App.Evaluator.Builtins (builtinFnsMap)
import App.Evaluator.Errors (EvalError(..), LexicalError(..))
import App.Parser.Common (var)
import App.SyntaxTree.Common (Module, QVar(..), QVarOp(..), Var(..), VarOp(..), preludeModule)
import App.SyntaxTree.FnDef (Associativity(..), BuiltinFnInfo, FnBody(..), FnDef(..), FnInfo(..), Object(..), OpInfo, Precedence(..), Scope(..))
import App.SyntaxTree.Pattern (Pattern(..))
import Bookhound.Parser (runParser)
import Bookhound.ParserCombinators (is)
import Bookhound.Utils.Array (hasSome)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.HashMap as HashMap
import Data.List as List
import Data.Set as Set
import Data.String (Pattern(..)) as String
import Data.String (stripSuffix)
import Data.Tree.Zipper (Loc, fromTree, toTree)

type LocalFormulaCtx =
  { tableData :: HashMap Cell CellValue
  , fnsMap :: HashMap QVar FnInfo
  , operatorsMap :: HashMap QVarOp OpInfo
  , aliasedModulesMap :: HashMap (Module /\ Module) (Set Module)
  , importedModulesMap :: HashMap Module (Set Module)
  , modules :: Set Module
  , localFnsMap :: HashMap (Scope /\ Var) FnInfo
  , argsMap :: HashMap (Scope /\ Var) FnInfo
  , module' :: Module
  , fnInfo :: Maybe FnInfo
  , scope :: Scope
  , scopeLoc :: Loc Scope
  , lambdaCount :: Int
  }

type EvalM a = ExceptT EvalError (State LocalFormulaCtx) a

registerBindings :: Array FnDef -> EvalM Unit
registerBindings bindings = do
  { scope, scopeLoc } <- get
  let (Scope maxScope) = fromMaybe scope $ maximum $ toTree scopeLoc
  let scopes = Scope <<< (_ + maxScope) <$> (1 .. length bindings)
  traverse_ (case _ of (n /\ x) -> registerLocalFn n x)
    ((NonEmptyArray.toArray scopes) `Array.zip` bindings)
  modify_ \st -> st
    { scopeLoc = appendChildren (mkLeaf <$> List.fromFoldable scopes)
        st.scopeLoc
    }

tailCallWrapper :: EvalM Object -> EvalM Object
tailCallWrapper action = untilLoopEnd \argObjs -> do
  when (hasSome argObjs) do
    { fnInfo } <- get
    (put =<< getNewFnState (unsafeFromJust fnInfo) (Object' <$> argObjs))
  action

untilLoopEnd :: (Array Object -> EvalM Object) -> EvalM Object
untilLoopEnd action = [] # tailRecM \args -> action args <#> case _ of
  LoopObj x -> Loop x
  x -> Done x

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
    lookupVar n = HashMap.lookup (n /\ fnName) localFnsMap
    lookupArg n = HashMap.lookup (n /\ fnName) argsMap
    lookupVarOrArg n = HashMap.lookup (n /\ fnName)
      (HashMap.union localFnsMap argsMap)
    childrenLookup = lookupVar <$> childrenValues scope scopeLoc
    siblingsLookup = lookupVar <$> siblingsValues scope scopeLoc
    argsLookup = lookupArg <$> nodeValues scope scopeLoc
    freeVarsLookup = lookupVarOrArg <$> ancestorsValues scope scopeLoc
  except
    $ note (LexicalError' $ UnknownValue $ QVar Nothing fnName)
    $ findMap identity
        (childrenLookup <> argsLookup <> siblingsLookup <> freeVarsLookup)

lookupModuleFn :: QVar -> EvalM FnInfo
lookupModuleFn qVar@(QVar fnModule fnName) = do
  st <- get
  let fns = getAvailableFns QVar (fnModule /\ fnName) st
  except
    $ note (LexicalError' $ UnknownValue qVar)
    $ findMap (flip HashMap.lookup st.fnsMap) fns

lookupBuiltinFn :: QVar -> EvalM BuiltinFnInfo
lookupBuiltinFn qVar@(QVar fnModule fnName) = do
  st <- get
  let fns = getAvailableFns QVar (fnModule /\ fnName) st
  except
    $ note (LexicalError' $ UnknownValue qVar)
    $ findMap (flip HashMap.lookup builtinFnsMap) fns

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
    $ findMap (flip HashMap.lookup st.operatorsMap) ops

insertFnDef
  :: Scope
  -> FnDef
  -> HashMap (Scope /\ Var) FnInfo
  -> HashMap (Scope /\ Var) FnInfo
insertFnDef scope (FnDef fnName params returnType doc body) =
  HashMap.insert (scope /\ fnName) fnInfo
  where
  fnInfo = FnInfo
    { id: Nothing
    , params
    , body
    , scope
    , argsMap: HashMap.empty
    , returnType
    , doc
    }

getNewFnState :: FnInfo -> Array FnBody -> EvalM LocalFormulaCtx
getNewFnState
  fnInfo@(FnInfo { id: maybeFnId, scope, params, argsMap })
  fnArgs =
  do
    st <- get
    let
      newArgsMap = HashMap.union argsMap
        $ HashMap.union argBindings st.argsMap
    pure $ case maybeFnId of
      Just { fnModule } ->
        st
          { argsMap = newArgsMap
          , localFnsMap = HashMap.empty
          , module' = fnModule
          , fnInfo = Just fnInfo
          , scope = zero
          , scopeLoc = fromTree $ mkLeaf zero
          }
      Nothing -> st
        { argsMap = newArgsMap
        , scope = scope
        , fnInfo = Just fnInfo
        , scopeLoc = goToNode scope st.scopeLoc
        }
  where
  argBindings = HashMap.fromArray
    $ rmap
        ( FnInfo
            <<<
              { id: Nothing
              , body: _
              , scope
              , params: []
              , argsMap: HashMap.empty
              , returnType: Nothing
              , doc: mempty
              }
        )
    <$> Array.zip ((scope /\ _) <<< fst <$> params) args
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

getAvailableFns
  :: forall a b
   . Ord b
  => (Maybe Module -> a -> b)
  -> (Maybe Module /\ a)
  -> LocalFormulaCtx
  -> Set b
getAvailableFns ctor (fnModule /\ fnName) st =
  Set.map (flip ctor fnName <<< pure) $ getAvailableModules fnModule st

getAvailableModules :: Maybe Module -> LocalFormulaCtx -> Set Module
getAvailableModules
  fnModule
  { module', modules, importedModulesMap, aliasedModulesMap } =
  case fnModule of
    Just alias ->
      Set.union (Set.filter (_ == alias) modules)
        $ fromMaybe Set.empty
        $ HashMap.lookup (module' /\ alias) aliasedModulesMap
    Nothing -> Set.insert module'
      $ fromMaybe Set.empty
      $ HashMap.lookup module' importedModulesMap

getAvailableAliases :: LocalFormulaCtx -> Set Module
getAvailableAliases { module', aliasedModulesMap } =
  Set.fromFoldable
    $ map snd
    $ filter (uncurry filterAliases)
    $ HashMap.keys aliasedModulesMap
  where
  filterAliases fnModule _ =
    fnModule == module'

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
varFn = FnVar <<< QVar (Just preludeModule) <<< Var

lambdaId :: Int -> String
lambdaId n = "__lambda__" <> show n
