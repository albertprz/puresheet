module App.Components.Table.Formula where

import FatPrelude

import App.CSS.ClassNames (invalidFormula, unknownFormula, validFormula)
import App.Components.Table.Cell (Cell)
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Tecton (ClassName)

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

type Formula = { formulaText :: String, affectedCells :: NonEmptySet Cell }

newtype FormulaId = FormulaId Int

data FormulaState
  = ValidFormula
  | InvalidFormula
  | UnknownFormula

derive instance Eq FormulaState
derive instance Eq FormulaId
derive instance Ord FormulaId
derive newtype instance Semiring FormulaId
