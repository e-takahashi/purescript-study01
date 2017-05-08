module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

--import C04.S15 (isAllTrue, count, reverse)
--import C05.S5 (factorial, pascalRule)
import C05.S9 (sameCity, person1, person2, person3, fromSingleton)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
{-
  log $ show $ isAllTrue [true, true, true]
  log $ show $ isAllTrue [true, false, true]
  log $ show $ count (\x -> x `mod` 2 == 0) [1,2,3,4,5]
  log $ show $ reverse ["a","b","c"]
  log $ show $ factorial 5
  log $ show $ pascalRule 1 1
  log $ show $ pascalRule 2 2
  log $ show $ pascalRule 5 3
  log $ show $ pascalRule 5 8
-}
  log $ show $ sameCity person1 person2
  log $ show $ sameCity person1 person3
  log $ show $ fromSingleton "a" ["b"]
  log $ show $ fromSingleton "a" []
  log $ show $ fromSingleton "a" ["x","y","z"]
    
  
  
