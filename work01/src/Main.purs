module Main where

import Prelude
import Euler (answer)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

--main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log ("The answer is " <> show answer)
