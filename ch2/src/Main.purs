module Main where

import Math (pi, pow)
import Prelude

newtype Radius = Radius Number
newtype Area = Area Number

instance showArea :: Show Area where
  show (Area n) = show n

circleArea :: Radius -> Area
circleArea (Radius r) = Area $ pi * r `pow` 2.0
