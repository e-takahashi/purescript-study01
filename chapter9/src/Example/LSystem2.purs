module Example.LSystem2 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (concatMap, foldM)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, lineTo, moveTo
                        , getContext2D, getCanvasElementById
                       , setFillStyle, fillPath, closePath
                       , setShadowOffsetX, setShadowOffsetY
                       , setShadowBlur, setShadowColor)
import Math as Math
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, random)

lsystem :: forall a m s. Monad m =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s m = go (concatMap prod s) (m - 1)

lsystemA :: forall a. Array a -> (a -> Array a) -> Int -> Array a
lsystemA init prod 0 = init
lsystemA init prod n = lsystemA (concatMap prod init) prod (n-1)

lsystemB :: forall a m s. Monad m => (s -> a -> m s) -> s -> Array a -> m s
lsystemB = foldM

type Angle = Number

--data Alphabet = L | R | F
--data Alphabet = L Angle | R Angle | F
--data Alphabet = L | R | F | M
data Alphabet = L | R | F Boolean

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

--main :: Eff (canvas :: CANVAS) Unit
main :: Eff (canvas :: CANVAS, console :: CONSOLE, random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
--    agl = Math.pi / 3.0
--    agl = Math.pi / 3.5

    initial :: Sentence
--    initial = [F, R agl, R agl, F, R agl, R agl, F, R agl, R agl]
--    initial = [M]
    initial = [F false]

    productions :: Alphabet -> Sentence
    productions L = [L]
--    productions (L a) = [L a]
    productions R = [R]
--    productions (R a) = [R a]
--    productions F = [F, L, F, R, R, F, L, F]
--    productions F = [F, L agl, F, R agl, R agl, F, L agl, F]
--    productions F = [F, L, F, R, F, R, R, F, L, F]
--    productions F = [F, L, F, R, F]
--    productions F = [F, L, M, L, F, R, M, R, F, R, M, R, F, L, M, L, F]
--    productions M = [M, R, F, R, M, L, F, L, M, L, F, L, M, R, F, R, M]
    productions (F true) = [F true, L, F false, L, F true, R, F false, R, F true, R, F false, R, F true, L, F false, L, F true]
    productions (F false) =
      map (\a -> case a of
              F b -> F (not b)
              L -> R
              R -> L) (productions (F true))

    interpret :: State -> Alphabet -> Eff (canvas :: CANVAS, console :: CONSOLE, random :: RANDOM) State
--    interpret state L = pure $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state L = do
      _ <- log "L"
      pure $ state { theta = state.theta - Math.pi / 3.0 }
--    interpret state (L a) = pure $ state { theta = state.theta - a }
--    interpret state R = pure $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state R = do
      _ <- log "R"
      pure $ state { theta = state.theta + Math.pi / 3.0 }
--    interpret state (R a) = pure $ state { theta = state.theta + a }
--    interpret state L = pure $ state { theta = state.theta - Math.pi / 2.5 }
--    interpret state R = pure $ state { theta = state.theta + Math.pi / 2.5 }
{-
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      _ <- lineTo ctx x y
      pure { x, y, theta: state.theta }
    interpret state M = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      _ <- lineTo ctx x y
      pure { x, y, theta: state.theta }
-}
    interpret state (F _) = do
      _ <- log "F"
      r <- random
      let x = state.x + Math.cos state.theta * 1.5 * r
          y = state.y + Math.sin state.theta * 1.5 * r
      _ <- lineTo ctx x y
      pure { x, y, theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  _ <- setFillStyle "#00FF00" ctx

  _ <- setShadowOffsetX 10.0 ctx
  _ <- setShadowOffsetY 10.0 ctx
  _ <- setShadowBlur 10.0 ctx
  _ <- setShadowColor "#000F00" ctx

  _ <- moveTo ctx initialState.x initialState.y

--  _ <- fillPath ctx $ lsystem initial productions interpret 5 initialState
--  _ <- fillPath ctx $ lsystemB interpret initialState $ lsystemA initial productions 5
  _ <- fillPath ctx $ lsystemB interpret initialState $ lsystemA initial productions 4

  closePath ctx
  
