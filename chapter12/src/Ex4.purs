module Ex4 where

import Prelude

import Data.Foreign 
import Data.Function.Uncurried (Fn3, runFn3)
import Control.Monad.Eff
import Control.Monad.Eff.Console (log, logShow, CONSOLE)
import Data.Either
import Control.Monad.Except

import Data.Show
import Data.Foldable (foldr)

import Control.Plus
import Data.NonEmpty
import Data.List.Types 
import Control.Monad.Cont.Trans
import Data.Identity

import Types (Async)
import Files
import Control.Parallel.Class
import Data.Traversable (traverse)
import Data.Array (concat, nub)

foreign import stringifyImpl :: Foreign -> String
stringify :: forall a. a -> String
stringify x = stringifyImpl (toForeign x)

-- http://blog.ndk.io/purescript-ffi.html

type Error' = String

foreign import readJSONImpl :: forall a.
                               Fn3
                               String
                               (a -> Either Error' a)
                               (Error' -> Either Error' a)
                               (Either Error' a)

readJSON :: forall a. String -> F a
readJSON s =
  case f s of
    Right a -> pure a
--    Left  e -> throwError (NonEmptyList ((JSONError e) :| empty))
    Left  e -> throwError (NonEmptyList (singleton (JSONError e)))
  where
    f s = runFn3 readJSONImpl s Right Left

----------------------------------------

newtype FormData =  FormData {references :: Array String}

instance showFormData :: Show FormData where
  show (FormData ss) = "{references:[" <> s <> "]}" where
    s = foldr (\a b -> a <> "," <> b) "" ss.references

testd :: FormData
testd = FormData {references: ["a","b"]}
tests = stringify testd :: String

type FilePath = String

collect :: forall eff. FilePath -> ExceptT ErrorCode (Async (console::CONSOLE, fs::FS|eff)) (Array String)
collect p = do
  content <- readFileContEx p
  case runExcept (readJSON content :: F FormData) of
    Left e -> throwError (show e)
    Right (FormData refs) -> do
      refs2 <- sequential $ traverse (\r -> parallel (collect r)) refs.references
      pure $ nub $ refs.references <> concat refs2
  
