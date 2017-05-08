module Ex11 where

import Data.Array (length, head, tail)
import Data.Maybe (Maybe)

myLength :: forall a. Array a -> Int
myLength a = length a

myIntHead :: Array Int -> Maybe Int
myIntHead a = head a

myStringTail :: Array String -> Maybe (Array String)
myStringTail a = tail a

