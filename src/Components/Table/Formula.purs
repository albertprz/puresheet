module App.Components.Table.Formula where

import FatPrelude

import App.CSS.ClassNames (invalidFormula, unknownFormula, validFormula)
import App.Components.Table.Cell (Cell)
import Data.Array as Array
import Data.HashMap as HashMap
import Data.List as List
import Data.Set as Set
import Data.Set.NonEmpty (toSet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree (Forest)
import Data.Tree.Zipper (Loc, children, fromTree, parents, toTree, value)
import Tecton (ClassName)

type DependenciesCtx r =
  { tableDependencies :: HashMap Cell (NonEmptySet FormulaId)
  , formulaCache :: HashMap FormulaId Formula
  | r
  }

getDependencies
  :: forall r
   . DependenciesCtx r
  -> NonEmptySet Cell
  -> Set Cell
  -> Either DependencyError (Forest FormulaId)
getDependencies ctx affectedCells formulaCells =
  children <$> dependenciesTreeHelper newCtx (fromTree $ mkLeaf formulaId)
  where
  formulaId = newFormulaId $ HashMap.keys ctx.formulaCache
  newCtx = ctx
    { formulaCache = HashMap.insert formulaId
        { formulaText: mempty
        , affectedCells
        , startingCell: minimum1 affectedCells
        }
        ctx.formulaCache
    , tableDependencies = HashMap.unionWith (<>)
        (toDependenciesMap formulaId formulaCells)
        ctx.tableDependencies
    }

dependenciesTreeHelper
  :: forall r
   . DependenciesCtx r
  -> Loc FormulaId
  -> Either DependencyError (Loc FormulaId)
dependenciesTreeHelper ctx loc =
  if isRecursive then
    Left CycleDependency
  else
    flip appendChildrenLocs loc
      <$> traverse (dependenciesTreeHelper ctx)
        (fromTree <<< mkLeaf <$> formulaDeps)
  where
  formulaId = value loc
  { affectedCells } = unsafeFromJust $ HashMap.lookup formulaId ctx.formulaCache
  formulaDeps = List.fromFoldable
    $ foldMap toSet
    $ filterMap
        (flip HashMap.lookup ctx.tableDependencies)
        (Array.fromFoldable affectedCells)
  isRecursive = elem formulaId (value <$> parents loc)
  appendChildrenLocs = appendChildren <<< map toTree

toDependenciesMap
  :: FormulaId -> Set Cell -> HashMap Cell (NonEmptySet FormulaId)
toDependenciesMap formulaId cells =
  HashMap.fromFoldable
    $ Set.map (_ /\ NonEmptySet.singleton formulaId) cells

newFormulaId :: forall t. Foldable t => t FormulaId -> FormulaId
newFormulaId formulaIds = maybe zero inc $ maximum formulaIds

formulaStateToClass :: FormulaState -> ClassName
formulaStateToClass = case _ of
  ValidFormula -> validFormula
  InvalidFormula -> invalidFormula
  UnknownFormula -> unknownFormula

type Formula =
  { formulaText :: String
  , affectedCells :: NonEmptySet Cell
  , startingCell :: Cell
  }

newtype FormulaId = FormulaId Int

data FormulaState
  = ValidFormula
  | InvalidFormula
  | UnknownFormula

derive instance Eq FormulaState
derive instance Eq FormulaId
derive instance Ord FormulaId
derive newtype instance Semiring FormulaId
derive instance Newtype FormulaId _
instance Show FormulaId where
  show = show <<< unwrap

instance Hashable FormulaId where
  hash = unwrap

data DependencyError =
  CycleDependency

derive instance Eq DependencyError

instance Show DependencyError where
  show CycleDependency = "Cicle in formula cell dependencies"
