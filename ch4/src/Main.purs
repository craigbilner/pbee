module Main where

import Prelude

abs :: Int -> Int
abs n
  | n < 0     = n * -1
  | otherwise = n

isEven :: Int -> Boolean
isEven n = testEveness $ abs n
  where testEveness x
          | x < 0  = false
          | x == 0 = true
          | otherwise = testEveness $ x - 2
