module Ex4 where

import Prelude

--import Data.Foreign (Foreign, F, toForeign, MultipleErrors)
import Data.Foreign 
import Data.Function.Uncurried (Fn3, runFn3)
import Control.Monad.Eff
import Data.Either
import Control.Monad.Except
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Show
import Data.Foldable (foldr)

import Control.Plus
import Data.NonEmpty
import Data.List.Types 

data FormData = FormData {references :: Array String}

instance showFormData :: Show FormData where
  show (FormData {references: ss}) = "{references:[" <> s <> "]}" where
    s = foldr (\a b -> a <> "," <> b) "" ss

testd :: FormData
testd = FormData {references: ["a","b"]}

foreign import stringifyImpl :: Foreign -> String
stringify :: forall a. a -> String
stringify x = stringifyImpl (toForeign x)

foreign import readJSONImpl :: forall a b. Fn3 String (a -> Unit) (b -> Unit) Unit

readJSON0 :: forall a b. String -> (Either MultipleErrors a -> Unit) -> Unit
readJSON0 s k = runFn3 readJSONImpl s ok ng where
  ok = k <<< Right
  ng = k <<< Left

readJSON :: forall a. String -> F a
readJSON s = do
  pure readJSON0 s (\e -> case e of
                       Right a -> 

testExcept :: forall a. Int -> a -> F a
testExcept n a = do
  if n < 10
    then throwError (NonEmptyList ((JSONError "small") :| empty))
    else pure a


