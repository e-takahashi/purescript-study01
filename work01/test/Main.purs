module Test.Main where

import Prelude
--import Control.Monad.Eff (Eff)
--import Control.Monad.Eff.Console (CONSOLE, log)
import Euler (answer)
import Test.Assert (assert)

--main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  assert (answer == 233168)
  
