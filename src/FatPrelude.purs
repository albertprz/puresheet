module FatPrelude
  ( module Prelude
  , module ArrayUtils
  , module CharUtils
  , module Data.Map
  , module Data.Set
  , module Maybe
  , module Array
  , module Tuple.Nested
  , module Type.Prelude
  , module Effect
  , module Unit
  , module Function
  , module Char
  , module CodeUnits
  , module AffClass
  , module EffClass
  ) where

import Prelude

import App.Utils.ArrayUtils (class Range, range, (..)) as ArrayUtils
import App.Utils.CharUtils (nextChar, prevChar) as CharUtils
import Data.Array hiding ((..), range) as Array
import Data.Char (fromCharCode, toCharCode) as Char
import Data.Function hiding (apply) as Function
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as Maybe
import Data.Set (Set)
import Data.String.CodeUnits (fromCharArray, toCharArray) as CodeUnits
import Data.Tuple.Nested ((/\)) as Tuple.Nested
import Data.Unit (Unit, unit) as Unit
import Effect (Effect, forE, foreachE, untilE, whileE)
import Prim hiding (Row) as Prim
import Type.Prelude (Proxy(..))
import Effect.Aff.Class (class MonadAff, liftAff) as AffClass
import Effect.Class (class MonadEffect, liftEffect) as EffClass
