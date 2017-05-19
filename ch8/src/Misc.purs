module Misc where

import Prelude (bind, pure)
import Data.Maybe (Maybe)
import Data.Array (tail, head)

third :: forall a. Array a -> Maybe a
third xs = do
  withSecond <- tail xs
  withThird  <- tail withSecond
  third'     <- head withThird
  pure third'
