module App.AppM where

import FatPrelude

import App.AppStore (Store, StoreAction)
import App.Routes (Route)
import Halogen.Router.Class (class MonadNavigate, class MonadRouter)
import Halogen.Router.Trans.Hash (RouterT)
import Halogen.Store.Monad (class MonadStore, StoreT)

newtype AppM a = AppM (RouterT Route (StoreT StoreAction Store Aff) a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadStore StoreAction Store AppM
derive newtype instance MonadNavigate Route AppM
derive newtype instance MonadRouter Route AppM
