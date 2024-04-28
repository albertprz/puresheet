module App.Components.HeaderMenu where

import FatPrelude hiding (div)

import App.AppM (AppM)
import App.CSS.ClassNames (headerMenu, navButton)
import App.Routes (Route(..), allRoutes, useRouter')
import App.Utils.HTML (materialIcon)
import Data.Array as Array
import Halogen (Component, Slot)
import Halogen.HTML (HTML, button)
import Halogen.HTML.Elements (div)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_)
import Halogen.Hooks as Hooks

component :: forall q. Component q Unit Unit AppM
component = Hooks.component \_ _ -> Hooks.do
  current /\ { navigate } <- useRouter'
  Hooks.pure do
    div [ class_ headerMenu ]
      ( Array.fromFoldable
          $ map (renderButton navigate)
          $ find (_ /= current) allRoutes
      )

renderButton :: forall w i. (Route -> i) -> Route -> HTML w i
renderButton navigate route =
  button [ class_ navButton, onClick \_ -> navigate route ]
    [ materialIcon icon ]
  where
  icon = case route of
    SpreadsheetView -> "grid_on"
    ExplorerView _ -> "functions"

type HeaderMenuSlot = forall q. Slot q Unit Unit

_headerMenu :: Proxy "headerMenu"
_headerMenu = Proxy
