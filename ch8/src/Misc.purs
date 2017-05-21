module Misc where

import Prelude (bind, pure, (+), ($), (/), (<<<), discard)
import Data.Maybe (Maybe(..))
import Data.Array (tail, head, foldM, sort, nub)
import Data.List.Types (List(..), (:))
import Control.Monad (class Monad)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message, catchException, throw)
import Control.Monad.Eff.Console (CONSOLE, log)

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

safeDivide :: forall eff. Int -> Int -> Eff ( exception :: EXCEPTION | eff ) (Maybe Int)
safeDivide _ 0 = throw "cannot divide"
safeDivide a b = pure $ Just (a / b)

printException :: forall eff. Error -> Eff ( console :: CONSOLE | eff ) (Maybe Int)
printException e = do
  log (message e)
  pure Nothing

doSafeDivide :: forall eff. Int -> Int -> Eff ( console :: CONSOLE | eff ) (Maybe Int)
doSafeDivide a b = do
    result <- catchException printException $ safeDivide a b
    pure result
