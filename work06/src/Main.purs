module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Either (Either(..))
import Data.String (null)

type FilePath = String
type ErrorCode = String

readFileImpl :: forall eff
                . FilePath
                -> (String -> Eff (console :: CONSOLE | eff) Unit)
                -> (ErrorCode -> Eff (console :: CONSOLE | eff) Unit)
                -> Eff (console :: CONSOLE | eff) Unit
readFileImpl path onSuccess onFailure = do
  log $ "path=" <> path
  if null path
    then onFailure "not specified the path"
    else onSuccess "ok"

readFile :: forall eff
            . FilePath
            -> (Either ErrorCode String -> Eff (console :: CONSOLE | eff) Unit)
            -> Eff (console :: CONSOLE | eff) Unit
readFile path k = readFileImpl path (k <<< Right) (k <<< Left)

success :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
success msg = do
  log $ "success: " <> msg

failure :: forall eff. ErrorCode -> Eff (console :: CONSOLE | eff) Unit
failure msg = do
  log $ "failure: " <> msg

okng :: forall eff. Either ErrorCode String -> Eff (console :: CONSOLE | eff) Unit
okng (Right s) = success s
okng (Left  e) = failure e

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  readFile "hoge" okng
  readFile "" okng
  
  
