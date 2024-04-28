module App.Routes where

import FatPrelude hiding (optional, sum, (/))

import App.Editor.Suggestion (SuggestionTerm(..), showFullTerm)
import App.Parser.Common (module', qVar, qVarOp)
import Bookhound.Parser (runParser)
import Data.Generic.Rep (class Generic)
import Halogen.Hooks (Hook)
import Halogen.Hooks.HookM (HookM)
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.UseRouter (UseRouter, useRouter)
import Routing.Duplex (RouteDuplex', as, optional, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

data Route
  = SpreadsheetView
  | ExplorerView { selectedTerm :: Maybe SuggestionTerm }

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "SpreadsheetView": "spreadsheet" / noArgs
  , "ExplorerView": "explorer"
      ? { selectedTerm: optional <<< selectedTermDuplex }
  }
  where
  selectedTermDuplex = as showFullTerm suggestionToString

  suggestionToString =
    lmap (foldMap show) <<< runParser suggestionParser

  suggestionParser = OpSuggestion <$> qVarOp
    <|> (FnSuggestion <$> qVar)
    <|> (ModuleSuggestion <$> module')

allRoutes :: Array Route
allRoutes = [ SpreadsheetView, ExplorerView { selectedTerm: Nothing } ]

nextRoute :: Route -> Route
nextRoute = case _ of
  SpreadsheetView -> ExplorerView { selectedTerm: Nothing }
  ExplorerView _ -> SpreadsheetView

lastRoute :: Route -> Route
lastRoute = case _ of
  SpreadsheetView -> ExplorerView { selectedTerm: Nothing }
  ExplorerView _ -> SpreadsheetView

useRouter'
  :: forall m
   . MonadRouter Route m
  => Hook m (UseRouter Route)
       ( Tuple Route
           { navigate :: Route -> HookM m Unit
           , print :: Route -> HookM m String
           }
       )
useRouter' = lmap (fromMaybe SpreadsheetView) <$> useRouter

derive instance Eq Route
derive instance Ord Route
derive instance Generic Route _
