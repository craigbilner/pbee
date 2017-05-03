module Main where

import Prelude

newtype Complex = Complex
  { real      :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> ", " <> show imaginary

instance eqComplex :: Eq Complex where
  eq (Complex { real: r1, imaginary: i1 }) (Complex { real: r2, imaginary: i2 }) = r1 == r2 && i1 == i2
