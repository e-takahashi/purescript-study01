module Ex2 where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (kind Effect, Eff)
import Data.Function.Uncurried (Fn2, runFn2)
import Types (Async)

import Data.Maybe (Maybe(..))

import Control.Parallel.Class
import Control.Alt
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

type Milliseconds = Int

foreign import data TIMEOUT :: Effect

foreign import setTimeoutImpl :: forall eff. Fn2 Milliseconds (Eff (timeout :: TIMEOUT | eff) Unit) (Eff (timeout :: TIMEOUT | eff) Unit)

setTimeout :: forall eff. Milliseconds -> (Unit -> (Eff (timeout :: TIMEOUT | eff)) Unit) -> (Eff (timeout :: TIMEOUT | eff)) Unit
setTimeout ms k = runFn2 setTimeoutImpl ms (k unit)

setTimeoutCont :: forall eff. Milliseconds -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont ms = ContT (setTimeout ms)

toMaybe :: forall a eff
           . Async (timeout :: TIMEOUT | eff) a
           -> Async (timeout :: TIMEOUT | eff) (Maybe a)
toMaybe c = ContT (\k -> runContT c (\a -> k (Just a)))

setTimeoutCont2 :: forall a eff
                   . Milliseconds
                   -> Async (timeout :: TIMEOUT | eff) (Maybe a)
setTimeoutCont2 ms = ContT (\k -> runContT (setTimeoutCont ms) (\_ -> k Nothing))

alt1 :: forall a eff
        . Milliseconds
        -> Async (timeout::TIMEOUT|eff) a
        -> Async (timeout::TIMEOUT|eff) (Maybe a)
alt1 ms ca =
  sequential $ alt (parallel (toMaybe ca)) (parallel (setTimeoutCont2 ms))

timeout :: forall a eff
           . Milliseconds
           -> Async (timeout::TIMEOUT|eff) a
           -> Async (timeout::TIMEOUT|eff) (Maybe a)
timeout ms ca =
  sequential $
  (parallel (ContT (\k -> runContT ca (\a -> k (Just a)))))
  <|>
  (parallel (ContT (\k -> runContT (setTimeoutCont ms) (\_ -> k Nothing))))

delay1 :: forall eff
          . String
          -> Milliseconds
          -> Async (timeout :: TIMEOUT | eff) String
delay1 msg ms = do
  setTimeoutCont ms
  pure msg


