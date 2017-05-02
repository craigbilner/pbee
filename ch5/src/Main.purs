module Main where

import Prelude
import Data.Picture (Shape, makeCircle)

factorial :: Number -> Number
factorial 0.0 = 1.0
factorial n = n * factorial (n - 1.0)

choose :: Int -> Int -> Int
choose n k
  | n == k    = 1
  | otherwise = ((choose (n - 1) k) * n) / (n - k)

type Address =
  {
    street :: String
  , city   :: String
  }

type Person =
  {
    name    :: String
  , address :: Address
  }

livesInLA :: forall r1 r2.
          { address :: { city :: String | r1 } | r2 }
          -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _                                    = false

sameCity :: forall r1 r2 r3 r4 a. Eq a
         => { address :: { city :: a | r1 } | r2 }
         -> { address :: { city :: a | r3 } | r4 }
         -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _       [x] = x
fromSingleton default _   = default

testCircle :: Shape
testCircle = makeCircle { x: 0.0, y: 0.0 } 10.0
