module App.Utils.HTML where

import FatPrelude

import App.CSS.ClassNames (materialIcons)
import Halogen.HTML (HTML, i, text)
import Halogen.HTML.Properties (class_)

renderWhen :: forall w i. Boolean -> HTML w i -> HTML w i
renderWhen cond elem =
  if cond then elem else text mempty

materialIcon :: forall w i. String -> HTML w i
materialIcon iconName = i
  [ class_ materialIcons ]
  [ text iconName ]
