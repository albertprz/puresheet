module App.Components.Table.Formula where

import FatPrelude

import App.CSS.ClassNames (invalidFormula, unknownFormula, validFormula)
import App.Components.Table.Cell (Cell)
import Bookhound.Utils.UnsafeRead (unsafeFromJust)
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty (toSet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Tree (Forest)
import Data.Tree.Zipper (Loc, children, fromTree, parents, toTree, value)
import Tecton (ClassName)

type DependenciesCtx r =
  { tableDependencies :: Map Cell (NonEmptySet FormulaId)
  , formulaCache :: Map FormulaId Formula
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
  formulaId = newFormulaId $ Map.keys ctx.formulaCache
  newCtx = ctx
    { formulaCache = Map.insert formulaId
        { formulaText: mempty
        , affectedCells
        , startingCell: minimum1 affectedCells
        }
        ctx.formulaCache
    , tableDependencies = Map.unionWith (<>)
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
    (_ `appendChildrenLocs` loc)
      <$> traverse (dependenciesTreeHelper ctx)
        (fromTree <<< mkLeaf <$> formulaDeps)
  where
  formulaId = value loc
  { affectedCells } = unsafeFromJust $ Map.lookup formulaId ctx.formulaCache
  formulaDeps = List.fromFoldable
    $ foldMap toSet
    $ filterMap (_ `Map.lookup` ctx.tableDependencies)
        (Array.fromFoldable affectedCells)
  isRecursive = elem formulaId (value <$> parents loc)
  appendChildrenLocs = appendChildren <<< map toTree

toDependenciesMap :: FormulaId -> Set Cell -> Map Cell (NonEmptySet FormulaId)
toDependenciesMap formulaId cells =
  Map.fromFoldable
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
derive newtype instance Show FormulaId

data DependencyError =
  CycleDependency

derive instance Eq DependencyError

instance Show DependencyError where
  show CycleDependency = "Cicle in formula cell dependencies"
