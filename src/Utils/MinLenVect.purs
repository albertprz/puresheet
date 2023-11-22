module Data.FastVect.MinLenVect
        ( (:)
  , MinLenVect
  , append
  , cons
  , drop
  , empty
  , fromArray
  , fromUnsizedArray
  , fromUnsizedNonEmptyArray
  , generate
  , head
  , last
  , index
  , indexModulo
  , mapWithTerm
  , modify
  , reifyMinLenVect
  , replicate
  , set
  , singleton
  , snoc
  , splitAt
  , take
  , toArray
  , toNonEmptyArray
  , toVect
  , fromVect
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.FastVect.Common as Common
import Data.FastVect.FastVect (Vect)
import Data.FastVect.FastVect as Vect
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable)
import Data.Semigroup.Foldable as Foldable1
import Data.Semigroup.Traversable as Traversable1
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Prelude as Prelude
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype MinLenVect :: Int -> Type -> Type
-- | A Minimum Length Vector: A list-like data structure that encodes it's minimum length in the type, backed by an `Array`.
-- |
-- | ```
-- | vect :: MinLenVect 1 String
-- | vect = singleton "a"
-- | ```
-- |

newtype MinLenVect len elem = MinLenVect (Array elem)


instance (Show elem, Reflectable len Int) => Show (MinLenVect len elem) where
  show (MinLenVect elems) = "MinLenVect " <> show (Common.toInt (Common.term :: _ len)) <> " " <> show elems

derive newtype instance Eq elem => Eq (MinLenVect len elem)
derive newtype instance Ord elem => Ord (MinLenVect len elem)
derive newtype instance Functor (MinLenVect len)
instance Apply (MinLenVect len) where
  apply (MinLenVect fab) (MinLenVect a) = MinLenVect (Array.zipWith ($) fab a)

instance (Compare len Common.NegOne GT, Reflectable len Int) => Applicative (MinLenVect len) where
  pure = replicate (Proxy :: _ len)

instance (Compare len Common.NegOne GT, Reflectable len Int) => Bind (MinLenVect len) where
  bind vec f = distribute f <*> vec

instance (Compare len Common.NegOne GT, Reflectable len Int) => Monad (MinLenVect len)
derive newtype instance FunctorWithIndex Int (MinLenVect len)
derive newtype instance Foldable (MinLenVect len)
derive newtype instance FoldableWithIndex Int (MinLenVect len)

instance (Compare len Common.Zero GT) => Foldable1.Foldable1 (MinLenVect len) where
  foldMap1 f xs = Foldable1.foldMap1 f $ toNonEmptyArray xs
  foldr1 f xs = Foldable1.foldr1 f $ toNonEmptyArray xs
  foldl1 f xs = Foldable1.foldl1 f $ toNonEmptyArray xs

derive newtype instance Traversable (MinLenVect len)
derive newtype instance TraversableWithIndex Int (MinLenVect len)

instance (Compare len Common.Zero GT) => Traversable1.Traversable1 (MinLenVect len) where
  traverse1 = Traversable1.traverse1Default
  sequence1 xs = unsafeFromNonEmptyArray <$> (Traversable1.sequence1 $ toNonEmptyArray xs)
    where
    -- assumes the Traversable1 instance for NonEmptyArray keeps the same
    -- amount of elements
    unsafeFromNonEmptyArray = NEA.toArray >>> MinLenVect

instance (Compare len Common.NegOne GT, Reflectable len Int) => Distributive (MinLenVect len) where
  distribute :: forall a g. Functor g => g (MinLenVect len a) -> MinLenVect len (g a)
  distribute xss = generate (Proxy :: _ len) f
    where
    f :: forall i. Compare i Common.NegOne GT => Compare i len LT => Reflectable i Int => Proxy i -> g a
    f _ = index (Proxy :: _ i) <$> xss
  collect = collectDefault

instance Semigroup a => Semigroup (MinLenVect len a) where
  append = lift2 Prelude.append

instance (Compare len Common.NegOne GT, Reflectable len Int, Monoid a) => Monoid (MinLenVect len a) where
  mempty = pure mempty

instance (Compare len Common.NegOne GT, Reflectable len Int, Semiring a) => Semiring (MinLenVect len a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance (Compare len Common.NegOne GT, Reflectable len Int, Ring a) => Ring (MinLenVect len a) where
  sub = lift2 sub

instance (Compare len Common.NegOne GT, Reflectable len Int, CommutativeRing a) => CommutativeRing (MinLenVect len a)


-- | Create a `MinLenVect` by replicating `len` times the given element
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- | ```
replicate :: forall len elem. Common.Replicate MinLenVect len elem
replicate proxy elem = MinLenVect $ Array.replicate (Common.toInt proxy) elem

-- | Creates the empty `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 0 String
-- | minLenVect = empty
-- | ```
empty :: forall elem. Common.Empty MinLenVect elem
empty = MinLenVect []

-- | Create a `MinLenVect` of one element.
-- |
-- | ```
-- | minLenVect :: MinLenVect 1 String
-- | minLenVect = singleton "a"
-- | ```
singleton :: forall elem. Common.Singleton MinLenVect elem
singleton elem = MinLenVect [ elem ]

-- | Append two `MinLenVect`s.
-- |
-- | ```
-- | as :: MinLenVect 300 String
-- | as = replicate (Common.term :: _ 300) "a"
-- |
-- | bs :: MinLenVect 200 String
-- | bs = replicate (Common.term :: _ 200) "b"
-- |
-- | cs :: MinLenVect 500 String
-- | cs = append as bs
-- | ```
append :: forall m n m_plus_n elem. Common.Append MinLenVect m n m_plus_n elem
append (MinLenVect xs) (MinLenVect ys) = MinLenVect (xs <> ys)

-- | Safely drop `m` elements from a `MinLenVect`.
-- | Will result in a compile-time error if you are trying to drop more elements than exist in the minLenVector.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | newMinLenVect :: MinLenVect 200 String
-- | newMinLenVect = drop (Common.term :: _ 100) minLenVect
-- | ```
drop :: forall m n m_plus_n elem. Common.Drop MinLenVect m n m_plus_n elem
drop proxy (MinLenVect xs) = MinLenVect (Array.drop (Common.toInt proxy) xs)

-- | Safely take `m` elements from a `MinLenVect`.
-- | Will result in a compile-time error if you are trying to take more elements than exist in the minLenVector.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | newMinLenVect :: MinLenVect 100 String
-- | newMinLenVect = take (Common.term :: _ 100) minLenVect
-- | ```
take :: forall m n m_plus_n elem. Common.Take MinLenVect m n m_plus_n elem
take proxy (MinLenVect xs) = MinLenVect (Array.take (Common.toInt proxy) xs)

foreign import modifyImpl :: forall n elem. Int -> (elem -> elem) -> MinLenVect n elem -> MinLenVect n elem

-- | Safely modify element `m` from a `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | newMinLenVect :: MinLenVect 100 String
-- | newMinLenVect = modify (Common.term :: _ 100) (append "b") minLenVect
-- | ```
modify :: forall m n elem. Common.Modify MinLenVect m n elem
modify proxy = modifyImpl (Common.toInt proxy)

-- | Safely set element `m` from a `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | newMinLenVect :: MinLenVect 100 String
-- | newMinLenVect = modify (Common.term :: _ 100) "b" minLenVect
-- | `
set :: forall m n elem. Common.Set MinLenVect m n elem
set proxy = modify proxy <<< const

-- | Split the `MinLenVect` into two sub minLenVectors `before` and `after`, where before contains up to `m` elements.
-- |
-- | ```
-- | minLenVect :: MinLenVect 10 String
-- | minLenVect = replicate (Common.term :: _ 10) "a"
-- |
-- | split ::
-- |   { after :: MinLenVect 7 String
-- |   , before :: MinLenVect 3 String
-- |   }
-- | split = splitAt (Common.term :: _ 3) minLenVect
-- | ```
splitAt :: forall m n m_plus_n elem. Common.SplitAt MinLenVect m n m_plus_n elem
splitAt proxy (MinLenVect xs) = { before: MinLenVect before, after: MinLenVect after }
  where
  { before, after } = Array.splitAt (Common.toInt proxy) xs

-- | Safely access the `n`-th modulo m element of a `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = indexModulo 5352523 minLenVect
-- | ```
indexModulo :: forall m elem. Common.IndexModulo MinLenVect m elem
indexModulo i = indexImpl (i `mod` Common.toInt (Proxy :: _ m))

foreign import indexImpl :: forall m elem. Int -> MinLenVect m elem -> elem

-- | Safely access the `i`-th element of a `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = index (Common.term :: _ 299) minLenVect
-- | ```
index :: forall m n elem. Common.Index MinLenVect m n elem
index = indexImpl <<< Common.toInt

-- | Safely access the head of a `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = head minLenVect
-- | ```
head :: forall m elem. Common.Head MinLenVect m elem
head = indexImpl 0

-- | Safely access the last element of a `MinLenVect`.
-- |
-- | ```
-- | minLenVect :: MinLenVect 300 String
-- | minLenVect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = last minLenVect
-- | ```
last :: forall m elem. Common.Last MinLenVect m elem
last = indexImpl $ Common.toInt (Common.term :: _ m) - 1

-- | Attempt to create a `MinLenVect` of at least a given size from an `Array`.
-- |
-- | ```
-- | fromArray (Common.term :: _ 3) ["a", "b", "c"] = Just (MinLenVect (Common.term :: _ 3) ["a", "b", "c"])
-- |
-- | fromArray (Common.term :: _ 3) ["a", "b", "c", "d", "e"] = Just (MinLenVect (Common.term :: _ 3) ["a", "b", "c", "d", "e"])
-- |
-- | fromArray (Common.term :: _ 4) ["a", "b", "c"] = Nothing
-- | ```
-- |
fromArray
  :: forall len elem
   . Reflectable len Int
  => Compare len Common.NegOne GT
  => Proxy len
  -> Array elem
  -> Maybe (MinLenVect len elem)
fromArray proxy array | Array.length array >= Common.toInt proxy =
  Just (MinLenVect array)
fromArray _ _ = Nothing

-- | Converts an `Array` to a `MinLenVect` of minimum length 0
fromUnsizedArray :: forall elem. Array elem -> MinLenVect 0 elem
fromUnsizedArray array = MinLenVect array

-- | Converts a `NonEmptyArray` to a `MinLenVect` of minimum length 1
fromUnsizedNonEmptyArray :: forall elem. NonEmptyArray elem -> MinLenVect 1 elem
fromUnsizedNonEmptyArray array = MinLenVect $ NEA.toArray array

-- | Converts the `MinLenVect` to an `Array`, effectively dropping the size information.
toArray
  :: forall len elem
   . Compare len Common.NegOne GT
  => MinLenVect len elem
  -> Array elem
toArray (MinLenVect arr) = arr

-- | Converts the `MinLenVect` to an `NonEmptyArray`, dropping most of the size information.
toNonEmptyArray
  :: forall len elem
   . Compare len Common.Zero GT
  => MinLenVect len elem
  -> NEA.NonEmptyArray elem
toNonEmptyArray (MinLenVect arr) = NonEmptyArray arr

toVect :: forall len elem
   . Reflectable len Int
  => Compare len Common.NegOne GT
  => Proxy len -> MinLenVect len elem -> Maybe (Vect len elem)
toVect proxy (MinLenVect arr) = Vect.fromArray proxy arr

fromVect :: forall len elem.
            Vect len elem -> MinLenVect len elem
fromVect = unsafeCoerce

-- | Attaches an element to the front of the `MinLenVect`, creating a new `MinLenVect` with size incremented.
-- |
-- | Note, the running time of this function is `O(n)`.
cons :: forall len len_plus_1 elem. Common.Cons MinLenVect len len_plus_1 elem
cons elem (MinLenVect arr) = MinLenVect (Array.cons elem arr)

-- | Attaches an element to the end of the `MinLenVect`, creating a new `MinLenVect` with size incremented.
snoc
  :: forall len len_plus_1 elem. Common.Snoc MinLenVect len len_plus_1 elem
snoc (MinLenVect arr) elem = MinLenVect (Array.snoc arr elem)

infixr 6 cons as :
infixr 6 index as !!
infixr 6 indexModulo as !%

-- | Applies a function to `Array` that takes a MinLenVector of arbitrary minimum length.
reifyMinLenVect
  :: forall elem r
   . Array elem
  -> (forall len. MinLenVect len elem -> r)
  -> r
reifyMinLenVect arr f = f (MinLenVect arr)

-- | Converts a function that takes any type-level integer value from 0 to len into a function that takes Int.
-- | Similar implementation to reifyType, but unsafe because type classes such as `Compare` are not ensured (so no export).
unsafeCoerceTerm
  :: forall len a
   . Proxy len
  -> ( forall i
        . Compare i Common.NegOne GT
       => Compare i len LT
       => Reflectable i Int
       => Proxy i
       -> a
     )
  -> Int
  -> a
unsafeCoerceTerm _ f i = internal f unit unit { reflectType: \_ -> i } Proxy
  where
  internal
    :: ( forall i
          . Compare i Common.NegOne GT
         => Compare i len LT
         => Reflectable i Int
         => Proxy i
         -> a
       )
    -> Unit
    -> Unit
    -> { reflectType :: Proxy _ -> Int }
    -> Proxy _
    -> a
  internal = unsafeCoerce

-- | Generate a `MinLenVect` of the given size by applying a function to each type level index.
generate :: forall len elem. Common.Generate MinLenVect len elem
generate _ f = MinLenVect $ map (\i -> unsafeCoerceTerm (Proxy :: _ len) f i) $ Array.range 0 (Common.toInt (Proxy :: _ len) - 1)

-- | Map a function over a `MinLenVect` with the type level index of each element.
mapWithTerm :: forall len elem elem'. Common.MapWithTerm MinLenVect len elem elem'
mapWithTerm f vect = mapWithIndex (\i elem -> unsafeCoerceTerm (Proxy :: _ len) f i elem) vect

instance Common.IsVect (MinLenVect n)
