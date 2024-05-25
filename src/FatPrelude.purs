module FatPrelude
  ( module X
  ) where

import App.Utils.Bounded (clampBounded, enumValues, getInBoundedRange, inBoundedRange, inRange, (..)) as X
import App.Utils.Char (fromUpper, nextChar, prevChar, toUpper, upperEndCode, upperStartCode) as X
import App.Utils.Foldable (findMapEither, intercalate1, maximum1, maximumBy1, minimum1, minimumBy1) as X
import App.Utils.Functor (filterByIndex, filterByIndexes, mapp, (<$$>)) as X
import App.Utils.Maybe (partialMaybe, unlessMaybe, unlessMaybe', unsafeFromJust, whenMaybe, whenMaybe') as X
import App.Utils.Monoid (unlessMonoid, whenMonoid, (<>?)) as X
import App.Utils.Number (abs, coalesce, dec, inc, neg, pos, zeroOrNeg, zeroOrPos) as X
import App.Utils.String (newline, showParensCsv, startsWith, str, tab, wrapBoth, wrapDoubleQuotes, wrapParens, wrapQuotes, wrapSquare) as X
import App.Utils.Tree (ancestorsValues, appendChildren, childrenValues, findValues, goToNode, mkLeaf, nodeValues, siblingsValues) as X
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchJust, liftEither, liftMaybe, withResource) as X
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, runExceptT, withExceptT) as X
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT, runMaybeT) as X
import Control.Monad.State (class MonadState, StateT(..), evalState, evalStateT, execState, execStateT, get, gets, mapState, mapStateT, modify, modify_, put, runState, runStateT, state, withState, withStateT) as X
import Control.Monad.Trans.Class (class MonadTrans, lift) as X
import Control.MonadPlus (class Alt, class Alternative, class MonadPlus, class Plus, alt, empty, guard, (<|>)) as X
import Data.Argonaut.Decode.Class (class DecodeJson) as X
import Data.Argonaut.Decode.Generic (genericDecodeJson) as X
import Data.Argonaut.Encode.Class (class EncodeJson) as X
import Data.Argonaut.Encode.Generic (genericEncodeJson) as X
import Data.Array (length) as X
import Data.Array.NonEmpty (NonEmptyArray) as X
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as X
import Data.Bitraversable (class Bifoldable, class Bitraversable, biall, biany, bifold, bifoldMap, bifoldl, bifoldr, bifor, bifor_, bisequence, bisequence_, bitraverse, bitraverse_, lfor, ltraverse, rfor, rtraverse) as X
import Data.Char (fromCharCode, toCharCode) as X
import Data.Either (Either(..), blush, choose, either, fromLeft, fromLeft', fromRight, fromRight', hush, isLeft, isRight, note, note') as X
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, downFrom, downFromIncluding, enumFromThenTo, enumFromTo, fromEnum, pred, succ, toEnum, toEnumWithDefaults, upFrom, upFromIncluding) as X
import Data.Filterable (class Compactable, class Filterable, cleared, compact, eitherBool, filter, filterMap, maybeBool, partition, partitionMap, separate) as X
import Data.Foldable (class Foldable, all, and, any, elem, find, findMap, fold, foldM, foldMap, foldl, foldr, for_, indexl, indexr, intercalate, lookup, maximum, maximumBy, minimum, minimumBy, notElem, null, or, product, sum, surround, surroundMap) as X
import Data.Function (applyN, on) as X
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex) as X
import Data.Generic.Rep (class Generic) as X
import Data.HashMap (HashMap) as X
import Data.HashSet (HashSet) as X
import Data.Hashable (class Hashable, class HashableRecord, hash, hashRecord) as X
import Data.List (List(..), (:)) as X
import Data.Map (Map) as X
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as X
import Data.Newtype (class Newtype, ala, alaF, collect, over, overF, overF2, un, under, under2, underF, underF2, unwrap, wrap) as X
import Data.Semigroup.Foldable (class Foldable1, fold1, foldMap1, foldMap1DefaultL, foldMap1DefaultR, foldl1, foldl1Default, foldr1, foldr1Default, for1_, intercalateMap, sequence1_, traverse1_) as X
import Data.Set (Set) as X
import Data.Set.NonEmpty (NonEmptySet) as X
import Data.String.CodeUnits (fromCharArray, toCharArray) as X
import Data.String.Common (joinWith, localeCompare, replace, replaceAll, split, trim) as X
import Data.Traversable (class Traversable, Accum, mapAccumL, mapAccumR, scanl, scanr, sequence, sequence_, traverse, traverse_) as X
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry) as X
import Data.Tuple.Nested (type (/\), curry3, curry4, get1, get2, get3, get4, over1, over2, over3, over4, tuple3, tuple4, uncurry3, uncurry4, (/\)) as X
import Debug (spy, spyWith) as X
import Effect (Effect, forE, foreachE, untilE, whileE) as X
import Effect.Aff (Aff, attempt, bracket, cancelWith, catchError, delay, fiberCanceler, finally, forkAff, launchAff, launchAff_, message, never, nonCanceler, parallel, runAff, runAff_, sequential, supervise, throwError, try) as X
import Effect.Aff.Class (class MonadAff, liftAff) as X
import Effect.Class (class MonadEffect, liftEffect) as X
import Effect.Exception (Error, error, throw) as X
import PSCI.Support (class Eval) as X
import PointFree ((#~), (#~~), (#~~~), (...>), (..>), (.>), (<.), (<..), (<...), (<~.), (<~..), (<~...), (<~~.), (<~~..), (<~~~.), (~$), (~...>), (~..>), (~.>), (~~$), (~~..>), (~~.>), (~~~$), (~~~.>)) as X
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as X
import Type.Prelude (Proxy(..)) as X
