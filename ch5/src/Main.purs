module Main where

import Prelude

factorial :: Number -> Number
factorial 0.0 = 1.0
factorial n = n * factorial (n - 1.0)
