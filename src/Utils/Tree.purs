module App.Utils.Tree where

import Prelude

import Data.Foldable (foldMap)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.Tree (Forest, Tree, mkTree)
import Data.Tree.Zipper (Loc, children, findFromRoot, fromTree, insertAfter, insertChild, node, parents, siblings, up, value)

nodeValues :: forall a. Eq a => a -> Loc a -> List a
nodeValues = findValues (pure <<< mkLeaf <<< value)

childrenValues :: forall a. Eq a => a -> Loc a -> List a
childrenValues = findValues children

siblingsValues :: forall a. Eq a => a -> Loc a -> List a
siblingsValues = findValues siblings

ancestorsValues :: forall a. Eq a => a -> Loc a -> List a
ancestorsValues val loc =
  findValues (map node <<< parents) val loc

findValues :: forall a. Eq a => (Loc a -> Forest a) -> a -> Loc a -> List a
findValues findFn val loc = value <<< fromTree <$>
  (foldMap findFn $ findFromRoot val loc)

goToNode :: forall a. Eq a => a -> Loc a -> Loc a
goToNode val loc =
  fromMaybe loc $ findFromRoot val loc

mkLeaf :: forall a. a -> Tree a
mkLeaf val =
  mkTree val mempty

appendChildren :: forall a. List (Tree a) -> Loc a -> Loc a
appendChildren Nil tree = tree
appendChildren (child : rest) loc =
  fromMaybe loc $ up (go rest firstElem)
  where
  firstElem = insertChild child loc
  go Nil elm = elm
  go (child' : rest') elm = go rest' $ insertAfter child' elm
