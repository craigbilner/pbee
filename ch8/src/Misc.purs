module Misc where

import Prelude (bind, pure, (+), ($), (<<<))
import Data.Maybe (Maybe)
import Data.Array (tail, head, foldM, sort, nub)
import Data.List.Types (List(..), (:))
import Control.Monad (class Monad)

third :: forall a. Array a -> Maybe a
third xs = do
  withSecond <- tail xs
  withThird  <- tail withSecond
  third'     <- head withThird
  pure third'

sums :: Array Int -> Array Int
sums xs = sort <<< nub $ foldM (\x y -> [x, y, x + y]) 0 xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  isMatch <- f x
  xs'     <- filterM f xs
  pure $ case isMatch of
    true  -> x : xs'
    false -> xs'
