module App.Components.Application where

import FatPrelude

import App.AppM (AppM)
import App.CSS.Application as Application
import App.Components.Explorer (_explorer)
import App.Components.Explorer as Explorer
import App.Components.HeaderMenu (_headerMenu)
import App.Components.HeaderMenu as HeaderMenu
import App.Components.Spreadsheet (_spreadsheet)
import App.Components.Spreadsheet as Spreadsheet
import App.Routes (Route(..), useRouter')
import Halogen (Component)
import Halogen.HTML (div_, slot_)
import Halogen.Hooks (useLifecycleEffect, useTickEffect)
import Halogen.Hooks as Hooks
import Tecton.Halogen (styleSheet)
import Web.DOM.Element (setAttribute)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window as Window

component :: forall q. Component q Unit Unit AppM
component = Hooks.component \_ _ -> Hooks.do
  route /\ { navigate } <- useRouter'
  useLifecycleEffect (Nothing <$ navigate SpreadsheetView)
  Hooks.captures { route } useTickEffect do
    toogleOverflow route *> mempty
  Hooks.pure do
    div_
      [ styleSheet Application.css
      , slot_ _headerMenu unit HeaderMenu.component unit
      , slot_ _spreadsheet unit Spreadsheet.component { route }
      , slot_ _explorer unit Explorer.component { route }
      ]

toogleOverflow :: forall m. MonadEffect m => Route -> m Unit
toogleOverflow route = liftEffect do
  body <- Document.body =<< Window.document =<< window
  traverse_ (setAttribute "style" ("overflow: " <> overflow))
    (toElement <$> body)
  where
  overflow = if route == SpreadsheetView then "hidden" else "auto"
