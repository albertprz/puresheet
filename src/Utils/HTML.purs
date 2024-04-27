module App.Utils.HTML where

import FatPrelude hiding (div)

import App.CSS.ClassNames (materialIcons)
import App.CSS.ClassNames as ClassNames
import CSSPrelude (searchInputContainer)
import Data.Array as Array
import Halogen.HTML (HTML, IProp, div, i, input, text)
import Halogen.HTML.Properties (class_)

renderWhen :: forall w i. Boolean -> HTML w i -> HTML w i
renderWhen cond elem =
  if cond then elem else text mempty

searchInput :: forall w i. Array (IProp _ i) -> HTML w i
searchInput props = div
  [ class_ searchInputContainer ]
  [ materialIcon "search"
  , input (Array.cons (class_ ClassNames.searchInput) props)
  ]

materialIcon :: forall w i. String -> HTML w i
materialIcon iconName = i
  [ class_ materialIcons ]
  [ text iconName ]
