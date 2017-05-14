module Main where

import Control.Applicative (class Applicative, pure)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Prelude (class Semiring, class Ring, class EuclideanRing, (+), (-), (/), (*), (<$>))

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
