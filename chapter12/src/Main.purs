module Main where

import Prelude

import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
--import Control.Monad.Trans.Class (lift)
import Data.Either (either)
--import Network.HTTP.Client (HTTP, get)
--import Types (Async)

--
import Control.Monad.Except.Trans (runExceptT)
import Network.HTTP.Client (HTTP, getEx)
import Files (FS, writeFileContEx)

{-
main :: Eff ( http :: HTTP
            , console :: CONSOLE
            ) Unit
main = async do
    response <- get "http://purescript.org"
    lift (either error log response)
  where
    async :: forall eff. Async eff Unit -> Eff eff Unit
    async = flip runContT pure
-}
main :: Eff ( http :: HTTP
            , console :: CONSOLE
            , fs :: FS
            ) Unit
main = runContT (runExceptT (do
                                content <- getEx "http://purescript.org"
                                writeFileContEx "hoge.txt" content
                                pure "ok"
                            )) (either error log) 
       
