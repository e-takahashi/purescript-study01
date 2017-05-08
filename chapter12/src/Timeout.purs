module Timeout where

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

{-
foreign import setTimeoutImpl :: forall eff. Fn2 Milliseconds (Unit -> Eff (timeout :: TIMEOUT | eff) Unit) (Eff (timeout :: TIMEOUT | eff) Unit)

setTimeout :: forall eff. Milliseconds -> (Unit -> Eff (timeout :: TIMEOUT | eff) Unit) -> (Eff (timeout :: TIMEOUT | eff) Unit)
setTimeout ms k = runFn2 setTimeoutImpl ms k

setTimeoutCont :: forall eff. Milliseconds -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont ms = ContT (setTimeout ms)
-}
foreign import setTimeoutImpl :: forall eff. Fn2 Milliseconds (Eff (timeout :: TIMEOUT | eff) Unit) (Eff (timeout :: TIMEOUT | eff) Unit)

setTimeout :: forall eff. Milliseconds -> (Unit -> (Eff (timeout :: TIMEOUT | eff)) Unit) -> (Eff (timeout :: TIMEOUT | eff)) Unit
setTimeout ms k = runFn2 setTimeoutImpl ms (k unit)

setTimeoutCont :: forall eff. Milliseconds -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont ms = ContT (setTimeout ms)

paraTout :: forall eff. Milliseconds -> Milliseconds -> Async (console :: CONSOLE, timeout :: TIMEOUT | eff) Unit
paraTout s1 s2 = do
  sequential $
    append <$>
    parallel (setTimeoutCont s1)
    <*> parallel (setTimeoutCont s2)

test1 :: Async (console :: CONSOLE) Unit -> Async (console :: CONSOLE) Unit -> Async (console :: CONSOLE) Unit 
test1 c1 c2 =
  sequential $ parallel c1 <|> parallel c2

cnt1 :: Milliseconds -> Async (console :: CONSOLE) Unit
cnt1 s = ContT (c s) where
  c s k = do
    logShow s
    k unit

cnt2 :: forall eff. Milliseconds -> Async (console :: CONSOLE, timeout :: TIMEOUT | eff) Unit
cnt2 ms = ContT (c ms) where
  c ms k = do
    log "ok"
    setTimeout ms k

{-
timeout :: forall a eff
           . Milliseconds
           -> Async (timeout :: TIMEOUT | eff) a
           -> Async (timeout :: TIMEOUT | eff) (Maybe a)
timeout ms k = do
  res <- sequential $
         (<|>) <$> parallel k <*> parallel (setTimeoutCont ms)
-}
