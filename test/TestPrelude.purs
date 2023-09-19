module TestPrelude
  ( module FatPrelude
  , module Test.QuickCheck
  , module Test.Spec
  , module Test.Spec.QuickCheck
  , module Control.Monad.Gen
  , prop
  ) where

import FatPrelude hiding (parallel, sequential)

import Control.Monad.Gen (class MonadGen, Size, chooseBool, chooseFloat, chooseInt, elements, filtered, frequency, oneOf, resize, sized, suchThat, unfoldable)
import Test.QuickCheck (class Arbitrary, class Testable, (===))
import Test.Spec (Spec, SpecT(..), after, afterAll, afterAll_, after_, around, aroundWith, around_, before, beforeAll, beforeAll_, beforeWith, before_, describe, describeOnly, evaluateExample, focus, hoistSpec, it, itOnly, mapSpecTree, parallel, pending, pending', sequential)
import Test.Spec.QuickCheck (quickCheck, quickCheck', quickCheckPure)

prop :: forall prop. Testable prop => String -> prop -> Spec Unit
prop str = it str <<< quickCheck
