module App.Components.Application where

import FatPrelude hiding (div)

import App.AppM (AppM)
import App.AppStore (Store, persistInLocalStorage)
import App.CSS.Application as Application
import App.Components.Explorer (_explorer)
import App.Components.Explorer as Explorer
import App.Components.HeaderMenu (_headerMenu)
import App.Components.HeaderMenu as HeaderMenu
import App.Components.Spreadsheet (_spreadsheet)
import App.Components.Spreadsheet as Spreadsheet
import App.Routes (Route(..), lastRoute, nextRoute, useRouter')
import App.Utils.Dom (withPrevent)
import App.Utils.Event (ctrlKey)
import App.Utils.KeyCode (KeyCode(..), mkKeyAction)
import Halogen (Component)
import Halogen.HTML (div, slot_)
import Halogen.HTML.Events (onKeyDown)
import Halogen.Hooks (HookM, useLifecycleEffect, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Query (SubscriptionId)
import Halogen.Query.Event (eventListener)
import Halogen.Store.Monad (class MonadStore, getStore)
import Tecton.Halogen (styleSheet)
import Web.DOM.Element (setAttribute)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window as Window

component :: forall q. Component q Unit Unit AppM
component = Hooks.component \_ _ -> Hooks.do
  route /\ { navigate } <- useRouter'

  useLifecycleEffect do
    subscriptionId <- subscribeWindowUnload
    pure $ Just $ Hooks.unsubscribe subscriptionId

  Hooks.captures { route } useTickEffect do
    toggleOverflow route *> mempty
  let
    handleKeyDown keyCode ev
      | ctrlKey ev && keyCode == CharKeyCode 'J' =
          withPrevent ev $ navigate $ nextRoute route
      | ctrlKey ev && keyCode == CharKeyCode 'K' =
          withPrevent ev $ navigate $ lastRoute route
      | otherwise = pure unit
  Hooks.pure do
    div
      [ onKeyDown $ mkKeyAction handleKeyDown
      ]
      [ styleSheet Application.css
      , slot_ _headerMenu unit HeaderMenu.component unit
      , slot_ _spreadsheet unit Spreadsheet.component { route }
      , slot_ _explorer unit Explorer.component { route }
      ]

subscribeWindowUnload
  :: forall m a. MonadEffect m => MonadStore a Store m => HookM m SubscriptionId
subscribeWindowUnload = do
  window <- liftEffect HTML.window
  Hooks.subscribe do
    eventListener
      (EventType "beforeunload")
      (Window.toEventTarget window)
      (const $ Just $ persistInLocalStorage =<< getStore)

toggleOverflow :: forall m. MonadEffect m => Route -> m Unit
toggleOverflow route = liftEffect do
  body <- Document.body =<< Window.document =<< window
  traverse_ (setAttribute "style" ("overflow: " <> overflow))
    (toElement <$> body)
  where
  overflow = if route == SpreadsheetView then "hidden" else "auto"
