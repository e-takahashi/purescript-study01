module Ex7 where

import Prelude

import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Network.HTTP.Client (HTTP, get)
import Types (Async)

import Control.Apply (lift2)
import Control.Parallel (parallel, sequential)
import Test.Unit.Console (TESTOUTPUT, print)

ex1 :: String -> String -> Eff ( http :: HTTP
                               , testOutput :: TESTOUTPUT
                               , console :: CONSOLE
                               ) Unit
{-
ex1 url = async do
  response <- get url
  lift (either error log response)
  where
    async :: forall eff. Async eff Unit -> Eff eff Unit
    async = flip runContT pure
-}
       
ex1 url1 url2 = async do
  response <- sequential $
              lift2 append
              <$> parallel (get url1)
              <*> parallel (get url2)
  lift (either error print response)
  lift $ print "hello"
  where
    async :: forall eff. Async eff Unit -> Eff eff Unit
    async = flip runContT pure

main :: Eff ( http :: HTTP
            , testOutput :: TESTOUTPUT
            , console :: CONSOLE
            ) Unit
main = do
  ex1 "http://purescript.org" "http://purescript.org/learn"
