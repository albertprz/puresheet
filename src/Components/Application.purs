module App.Components.Application where

import FatPrelude hiding (div)

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
import Halogen.Hooks (useLifecycleEffect)
import Halogen.Hooks as Hooks
import Tecton.Halogen (styleSheet)

component :: forall q. Component q Unit Unit AppM
component = Hooks.component \_ _ -> Hooks.do
  route /\ { navigate } <- useRouter'
  useLifecycleEffect (Nothing <$ navigate SpreadsheetView)
  Hooks.pure do
    div_
      [ styleSheet Application.css
      , slot_ _headerMenu unit HeaderMenu.component unit
      , slot_ _spreadsheet unit Spreadsheet.component { route }
      , slot_ _explorer unit Explorer.component { route }
      ]
