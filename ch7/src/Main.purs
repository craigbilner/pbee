module Main where

import Control.Applicative (class Applicative, pure)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Prelude (class Semiring, class Ring, class EuclideanRing, class Functor, class Show, (+), (-), (/), (*), (<$>), (<*>), ($), id, show)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))

liftPlus :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
liftPlus = lift2 (+)

liftMinus :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
liftMinus = lift2 (-)

liftDiv :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
liftDiv = lift2 (/)

liftMult :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
liftMult = lift2 (*)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing   = pure Nothing
combineMaybe (Just fa) = Just <$> fa

traverse' :: forall a b m t. Traversable t => Applicative m =>
             (a -> m b) -> t a -> m (t b)
traverse' f x = sequence $ f <$> x

sequence' :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequence' = traverse id

data Tree a =
    Leaf
  | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf                  = Leaf
  map f (Branch left v right) = Branch (f <$> left) (f v) (f <$> right)

instance foldableTree :: Foldable Tree where
  foldl _ i Leaf                  = i
  foldl f i (Branch left v right) = foldl f (f (foldl f i left) v) right
  foldr _ i Leaf                  = i
  foldr f i (Branch left v right) = foldr f (f v (foldr f i right)) left
  foldMap _ Leaf = mempty
  foldMap f (Branch left v right) = foldMap f left <> f v <> foldMap f right

instance traversableTree :: Traversable Tree where
  traverse _ Leaf                   = pure Leaf
  traverse f (Branch left v right)  = Branch <$> (traverse f left) <*> f v <*> (traverse f right)
  sequence = traverse id

instance showTree :: Show a => Show (Tree a) where
  show Leaf = ""
  show (Branch left v right) = show left <> " " <> show v <> " " <> show right
