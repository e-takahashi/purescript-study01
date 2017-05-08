module Ex11 where

import Data.Array (length)

myLength :: forall a. Array a -> Int
myLength a = length a

