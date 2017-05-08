module Example.Shapes where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, closePath, lineTo, moveTo, fillPath,
                        setFillStyle, arc, rect, getContext2D,
                        getCanvasElementById,
                        setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle "#0000FF" ctx

  _ <- fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }

  _ <- setStrokeStyle "#0000FF" ctx
  _ <- strokePath ctx $ rect ctx $ translate (0.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }

  _ <- setFillStyle "#00FF00" ctx

  _ <- fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , r: 50.0
    , start: Math.pi * 5.0 / 8.0
    , end: Math.pi * 2.0
    }

  _ <- setStrokeStyle "#00FF00" ctx
  _ <- strokePath ctx $ arc ctx $ translate 0.0 200.0
    { x: 300.0
    , y: 300.0
    , r: 50.0
    , start: Math.pi * 5.0 / 8.0
    , end: Math.pi * 2.0
    }

  _ <- setFillStyle "#FF0000" ctx

  _ <- fillPath ctx $ do
    _ <- moveTo ctx 300.0 260.0
    _ <- lineTo ctx 260.0 340.0
    _ <- lineTo ctx 340.0 340.0
    closePath ctx

  _ <- setStrokeStyle "#FF0000" ctx
  strokePath ctx $ do
    _ <- moveTo ctx 500.0 260.0
    _ <- lineTo ctx 460.0 340.0
    _ <- lineTo ctx 540.0 340.0
    closePath ctx
    
