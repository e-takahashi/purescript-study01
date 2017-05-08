module Main where

import Prelude
import Control.Monad.Eff.Console (logShow)
import Math (sqrt, pi)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

main = logShow (diagonal 3.0 4.0)

