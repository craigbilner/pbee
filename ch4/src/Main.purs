module Main where

import Prelude
import Data.Maybe
import Control.MonadZero (guard)
import Data.Array (filter, (..), length)
import Data.Array.Partial (head, tail)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.Int (toNumber, fromNumber)
import Math ((%), pow)
import Partial.Unsafe (unsafePartial)

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

sqr :: Number -> Number
sqr = flip pow 2.0

squareTheNumbers :: Array Number -> Array Number
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
