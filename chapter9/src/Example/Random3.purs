module Example.Random3 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
--import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, strokePath, fillPath, arc, setStrokeStyle,
                        setFillStyle, getContext2D, getCanvasElementById
                       , Context2D, save, restore)
import Math as Math
import Partial.Unsafe (unsafePartial)

import DOM (DOM)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (addEventListener, querySelector)

fillStrokePath :: forall eff
                  . Context2D
                  -> Eff (canvas :: CANVAS | eff) Context2D
                  -> Eff (canvas :: CANVAS | eff) Context2D
fillStrokePath ctx path = do
  _ <- save ctx
  _ <- fillPath ctx path
  _ <- strokePath ctx path
  restore ctx

main :: Eff (canvas :: CANVAS
            , dom :: DOM
            , console :: CONSOLE
            , random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle "#FF0000" ctx
  _ <- setStrokeStyle "#000000" ctx

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    log "Mouse clicked!"
    x <- random
    y <- random
    r <- random
    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , r     : r * 50.0
         , start : 0.0
         , end   : Math.pi * 2.0
         }
    fillStrokePath ctx path
