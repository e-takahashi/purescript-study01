module Study where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)

import Data.Maybe (Maybe(..))

f1 :: Int -> (String -> Eff (console :: CONSOLE) Unit) -> Eff (console :: CONSOLE) Unit
f1 n k = do
  k $ "[" <> show n <> "]"

cont1 :: Int -> ContT Unit (Eff (console :: CONSOLE)) String
cont1 n = ContT (f1 n)

f2 :: Int -> (String -> Eff (console :: CONSOLE) Unit) -> Eff (console :: CONSOLE) Unit
f2 n k = do
  k $ "[" <> show (n*2) <> "]"

cont2 :: Int -> ContT Unit (Eff (console :: CONSOLE)) String
cont2 n = ContT (f2 n)

fa :: Int -> (Int -> Eff (console :: CONSOLE) Unit) -> Eff (console :: CONSOLE) Unit
fa n k = do
  log $ "do something " <> show n
  k n

contA :: Int -> ContT Unit (Eff (console :: CONSOLE)) Int
contA n = ContT (fa n)

fb :: Int -> (Unit -> Eff (console :: CONSOLE) Unit) -> Eff (console :: CONSOLE) Unit
fb n k = do
  log ("timer " <> show n)
  k unit

contB :: Int -> ContT Unit (Eff (console :: CONSOLE)) Unit
contB n = ContT (fb n)

fc :: forall a
      . Int
      -> ContT Unit (Eff (console :: CONSOLE)) a
      -> (a -> Eff (console :: CONSOLE) Unit)
      -> Eff (console :: CONSOLE) Unit
      -> Eff (console :: CONSOLE) Unit
fc n c k1 k2 =
  if n < 0
  then runContT c k1
  else k2

fcc :: forall a
       . Int
       -> ContT Unit (Eff (console :: CONSOLE)) a
       -> (Maybe a -> Eff (console :: CONSOLE) Unit)
       -> Eff (console :: CONSOLE) Unit
fcc n c k = fc n c (k <<< Just) (k Nothing)

contC :: forall a. Int -> ContT Unit (Eff (console :: CONSOLE)) a -> ContT Unit (Eff (console :: CONSOLE)) (Maybe a)
contC n c = ContT (fcc n c)

contE :: Int -> ContT Unit (Eff (console :: CONSOLE)) Int
contE n = ContT (c n) where
  c n k = do
    log $ "contE " <> show n
    k (n*n)

fd :: forall a. Show a => (Maybe a) -> Eff (console :: CONSOLE) Unit
fd (Just a) = log $ "just " <> show a
fd Nothing  = log "nothing"

