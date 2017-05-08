module C05.S5 where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

pascalRule :: Int -> Int -> Int
pascalRule 1 _ = 1
pascalRule _ 1 = 1
pascalRule n k
  | n <= 0 = 0
  | k <= 0 = 0
  | k >  n = 0
  | k == 1 = 1
  | k == n = 1
  | otherwise = (pascalRule (n-1) (k-1)) + (pascalRule (n-1) k)

  
