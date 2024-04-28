module App.Utils.HTML where

import FatPrelude hiding (div)

import App.CSS.ClassNames (cellSyntax, functionSyntax, keywordSyntax, materialIcons, moduleSyntax, numberSyntax, operatorSyntax, regularSyntax, stringSyntax, symbolSyntax)
import App.CSS.ClassNames as ClassNames
import App.SyntaxTree.Common (QVar)
import App.SyntaxTree.FnDef (FnSig)
import App.Utils.SyntaxAtom (SyntaxAtom(..), condenseSyntaxAtoms, fnSigToSyntaxAtoms, syntaxAtomParser)
import Bookhound.Parser (runParser)
import CSSPrelude (searchInputContainer)
import Data.Array as Array
import Halogen.HTML (ClassName, HTML, IProp, div, i, input, span, text)
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

formulaElements :: forall a b. String -> Array (HTML a b)
formulaElements =
  syntaxAtomsToElements <<< fold <<< runParser syntaxAtomParser

fnSigElements :: forall a b. QVar -> FnSig -> Array (HTML a b)
fnSigElements =
  syntaxAtomsToElements <.. fnSigToSyntaxAtoms

syntaxAtomsToElements :: forall a b. Array SyntaxAtom -> Array (HTML a b)
syntaxAtomsToElements = map toElement <<< condenseSyntaxAtoms
  where
  toElement atom = span
    [ class_ $ syntaxAtomToClassName atom ]
    [ text $ show atom ]

syntaxAtomToClassName :: SyntaxAtom -> ClassName
syntaxAtomToClassName = case _ of
  Cell' _ -> cellSyntax
  Number' _ -> numberSyntax
  String' _ -> stringSyntax
  Char' _ -> stringSyntax
  Keyword _ -> keywordSyntax
  Symbol _ -> symbolSyntax
  Operator _ -> operatorSyntax
  Function _ -> functionSyntax
  Module _ -> moduleSyntax
  OtherText _ -> regularSyntax
