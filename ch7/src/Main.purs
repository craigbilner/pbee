module Main where

import Control.Applicative (class Applicative, pure)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Prelude (class Semiring, class Ring, class EuclideanRing, (+), (-), (/), (*), (<$>), ($), id)
import Data.Traversable (class Traversable, sequence, traverse)

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
