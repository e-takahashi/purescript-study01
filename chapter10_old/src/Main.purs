module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (null)

shout :: forall a. Show a => a -> String
shout a = show a <> "!!!"

hoge :: forall a. Ord a => Array a -> Boolean
hoge a = null a

geho :: forall a. a -> a -> a
geho a b = a

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ shout "hello"
  log $ shout $ hoge [1,2]
  
