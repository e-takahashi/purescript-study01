module Example.Rectangle where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById,
                       moveTo, lineTo, closePath, setStrokeStyle,
                       strokePath, arc, Context2D)
import Partial.Unsafe (unsafePartial)
import Math as Math
import Data.Array (foldM, head, tail, (..))
import Data.Int (toNumber)

type Point = { x :: Number, y :: Number }

renderPath :: forall eff
              . Context2D
              -> Array Point
              -> Eff (canvas :: CANVAS | eff) Unit
renderPath _ [] = pure unit
renderPath ctx ps =
  let fst = head ps
  in case fst of
    Nothing -> pure unit
    Just ph -> let rst = tail ps
               in case rst of
                 Nothing -> (moveTo ctx ph.x ph.y) >>= (\_ -> pure unit)
                 Just pt -> strokePath ctx $ do
                   _ <- moveTo ctx ph.x ph.y
                   _ <- foldM (\b p -> lineTo ctx p.x p.y) ctx pt
                   _ <- closePath ctx
                   pure unit

f1 :: Number -> Point
f1 x = {x:x, y:Math.sin (Math.pi * 2.0 * x)}
f2 :: Number -> Point
f2 x = {x:x, y:Math.cos (Math.pi * 2.0 * x)}

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setStrokeStyle "#FF0000" ctx
  _ <- renderPath ctx $ map (\p -> {x:p.x+10.0, y:p.y+200.0})
       $ map (\p -> {x:p.x*100.0, y:p.y*50.0})
       $ map (\x -> f1 ((toNumber x)/100.0)) (0..99)

  _ <- setStrokeStyle "#0000FF" ctx
  _ <- renderPath ctx $ map (\p -> {x:p.x+10.0, y:p.y+200.0})
       $ map (\p -> {x:p.x*100.0, y:p.y*50.0})
       $ map (\x -> f2 ((toNumber x)/100.0)) (0..99)

  _ <- renderPath ctx [
      {x: 200.0, y: 50.0},
      {x: 300.0, y: 100.0},
      {x: 250.0, y: 150.0},
      {x: 350.0, y: 200.0},
      {x: 200.0, y: 200.0},
      {x: 250.0, y: 50.0}
    ]

  _ <- setFillStyle "#0000FF" ctx

  _ <- fillPath ctx $ rect ctx
   { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }

  _ <- fillPath ctx $ do
    _ <- moveTo ctx 100.0 150.0
    _ <- lineTo ctx 0.0 150.0
    _ <- lineTo ctx 0.0 50.0
    _ <- lineTo ctx 100.0 50.0
    _ <- lineTo ctx 100.0 150.0
    _ <- lineTo ctx 200.0 150.0
    _ <- lineTo ctx 200.0 50.0
    _ <- lineTo ctx 100.0 50.0
    _ <- lineTo ctx 100.0 150.0
    closePath ctx

  _ <- setStrokeStyle "#0000FF" ctx
  _ <- strokePath ctx $ arc ctx
       { x: 400.0
       , y: 400.0
       , r : 50.0
       , start: Math.pi * 5.0 / 8.0
       , end: Math.pi * 2.0
       }
  strokePath ctx $ do
    _ <- moveTo ctx (400.0 + 50.0 * Math.cos (Math.pi * 5.0 / 8.0))
         (400.0 + 50.0 * Math.sin (Math.pi * 5.0 / 8.0))
    _ <- lineTo ctx 400.0 400.0
    lineTo ctx (400.0 + 50.0 * Math.cos (Math.pi * 2.0))
         (400.0 + 50.0 * Math.sin (Math.pi * 2.0))
    
