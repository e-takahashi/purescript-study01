module C04.S15 where

import Prelude
import Data.Foldable (foldl)

isAllTrue :: Array Boolean -> Boolean
isAllTrue = foldl (\b a -> a && b) true
    
count :: forall a. (a -> Boolean) -> Array a -> Int
count p = foldl (\b a -> b + (if p a then 1 else 0)) 0

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

