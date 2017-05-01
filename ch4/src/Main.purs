module Main where

import Prelude (($), (+))
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = count' p 0 xs
  where
  count' _  acc []  = acc
  count' p' acc xs' = if p $ unsafePartial head xs'
                      then count' p' (acc + 1) $ unsafePartial tail xs'
                      else count' p' acc $ unsafePartial tail xs'
