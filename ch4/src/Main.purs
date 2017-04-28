module Main where

import Prelude
import Data.Maybe
import Control.MonadZero (guard)
import Data.Array (filter, (..), cons, length, concat)
import Data.Array.Partial (head, tail)
import Data.Int (toNumber, fromNumber, pow, floor)
import Math ((%), sqrt)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (foldl)

abs :: Int -> Int
abs n
  | n < 0     = negate n
  | otherwise = n

isEven :: Int -> Boolean
isEven n = testEveness $ abs n
  where testEveness x
          | x < 0     = false
          | x == 0    = true
          | otherwise = testEveness $ x - 2

countEvens :: Array Int -> Int
countEvens [] = 0
countEvens xs = let
                  rem'   = fromNumber (toNumber (unsafePartial head xs) % toNumber 2)
                  others = countEvens $ unsafePartial tail xs
                in
                  case rem' of
                  Just 0    -> 1 + others
                  otherwise -> others

sqr :: Int -> Int
sqr = flip pow 2

squareTheNumbers :: Array Int -> Array Int
squareTheNumbers = map sqr

isPositive :: Number -> Boolean
isPositive n = n > 0.0

infixl 4 filter as <$?>

onlyPositives :: Array Number -> Array Number
onlyPositives xs = isPositive <$?> xs

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime = eq 1 <<< length <<< factors

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

safeSqrt :: Int -> Maybe Int
safeSqrt n = fromNumber $ sqrt $ toNumber n

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. (n - 1)
  b <- (a + 1) .. (n - 1)
  let a' = sqr a
  let b' = sqr b
  let c' = a' + b'
  let mc = safeSqrt c'
  guard $ case mc of
            Just c -> c < n
            _      -> false
  case mc of
    Just c -> pure [a, b, c]
    _      -> pure []

factorizations :: Int -> Array (Array Int)
factorizations a =
  if a <= 0
  then []
  else if a == 1
  then [[1]]
  else
    cons [1, a] $ factorizations' a 1
  where
    factorizations' n cond = concat $ do
      let uBound = floor $ sqrt $ toNumber n

      i <- if uBound >= (cond + 1)
           then (cond + 1) .. uBound
           else []

      guard $ case fromNumber $ (toNumber n) / (toNumber i) of
              Just f    -> f /= i
              otherwise -> false

      let j = n / i
      pure $ cons [i, j] $ map (cons i) $ factorizations' j i

allTrue :: Array Boolean -> Boolean
allTrue [] = false
allTrue xs = foldl (&&) true xs

-- foldl (==) false xs -> returns true
-- [false]
-- [false, ...true]
-- [false, true, false, false, ...]

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = count' p 0 xs
  where
  count' _  acc []  = acc
  count' p' acc xs' = if p $ unsafePartial head xs'
                      then count' p' (acc + 1) $ unsafePartial tail xs'
                      else count' p' acc $ unsafePartial tail xs'

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> cons x xs) []
