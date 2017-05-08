module Main where

import Prelude
import Data.Foldable
import Data.Monoid (class Monoid)

newtype Complex = Complex
  { real      :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> ", " <> show imaginary

instance eqComplex :: Eq Complex where
  eq (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = r1 == r2 && i1 == i2

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a as) = show a <> " " <> show as

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a1 as1) (NonEmpty a2 as2) = a1 == a2 && as1 == as2

instance semiGroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a1 as1) (NonEmpty _ as2) = NonEmpty a1 $ as1 <> as2

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a as) = NonEmpty (f a) $ map f as

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite a1) (Finite a2) = a1 == a2
  eq Infinite    Infinite    = true
  eq _           _           = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite a1) (Finite a2) = a1 `compare` a2
  compare Infinite    Infinite    = EQ
  compare Infinite    _           = GT
  compare _           Infinite    = LT

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = foldl f (f acc x) xs
  foldr f acc (NonEmpty x xs) = foldr f (f x acc) xs
  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldr f acc (OneMore x xs) = foldr f (f x acc) xs
  foldMap f (OneMore x xs) = f x <> foldMap f xs
