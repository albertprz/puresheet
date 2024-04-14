module App.Routes where

import FatPrelude hiding (sum, (/))

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Unfoldable (class Unfoldable1)
import Halogen.Hooks (Hook)
import Halogen.Hooks.HookM (HookM)
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.UseRouter (UseRouter, useRouter)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = SpreadsheetView
  | ExplorerView

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "SpreadsheetView": "spreadsheet" / noArgs
  , "ExplorerView": "explorer" / noArgs
  }

allRoutes :: forall @f. Unfoldable1 f => f Route
allRoutes = enumValues

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

instance Enum Route where
  succ = genericSucc
  pred = genericPred

instance Bounded Route where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Route where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
