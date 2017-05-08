module Main where

import Prelude
import Control.Monad.Eff (Eff, forE, runPure)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)
import Control.Monad.Eff.Random

import Data.Array
import Data.Maybe (Maybe(..))
import Data.List.Types (List(..),(:))
import Data.Int

-- 8.7

third :: forall a. Array a -> Maybe a
third x = do
  xs1 <- tail x
  xs2 <- tail xs1
  head xs2

sums :: Array Int -> Array Int
sums =
  sort <<< nub <<< concat <<< (foldM (\b a -> [map ((+) a) b, b]) [0])

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x:xs) = do
  b <- filterM f xs
  a <- f x
  pure $ if a then x:b else b

safeDivide :: forall eff. Int -> Int -> Eff (exception :: EXCEPTION|eff) Int
safeDivide _ 0 = throwException $ error "Dividion by zero"
safeDivide x y = pure (x / y)

estimatePi :: forall eff h. Int -> Eff (st::ST h, random :: RANDOM, exception :: EXCEPTION | eff) Number
estimatePi 0 = throwException $ error "Zero isnot allowed."
estimatePi n = do
  ref <- newSTRef 0
  forE 0 n \_ -> do
    x <- random
    y <- random
    _ <- modifySTRef ref \c ->
      if (x*x+y*y) <= 1.0 then (c+1) else c
    pure unit
  final <- readSTRef ref
  pure $ 4.0 * (toNumber final) / (toNumber n)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

