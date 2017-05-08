module Main where

import Prelude
import Node.ReadLine as RL
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.RWS (RWSResult(..), runRWS)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.GameState (GameState, initialGameState)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Game (game)
import Node.Yargs.Applicative (Y, runY, flag, yarg)
import Node.Yargs.Setup (usage)

import Control.Monad.Except.Trans (runExceptT)

runGame
  :: forall eff
   . GameEnvironment
  -> Eff ( exception :: EXCEPTION
         , readline :: RL.READLINE
         , console :: CONSOLE
         | eff
         ) Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " 2 interface

  let
    lineHandler
      :: GameState
      -> String
      -> Eff ( exception :: EXCEPTION
             , console :: CONSOLE
             , readline :: RL.READLINE
             | eff
             ) Unit
    lineHandler currentState input = do
      case runRWS (runExceptT (game (split (wrap " ") input))) env currentState of
        RWSResult state (Right _) written -> do
          for_ written log
          RL.setLineHandler interface $ lineHandler state
        RWSResult state (Left err) written -> do
          for_ (map (\e -> "error: " <> e) err) log
          RL.setLineHandler interface $ lineHandler state
{-
      case runRWS (game (split (wrap " ") input)) env currentState of
        RWSResult state _ written -> do
          for_ written log
          RL.setLineHandler interface $ lineHandler state
-}
      RL.prompt interface
      pure unit

  RL.setLineHandler interface $ lineHandler initialGameState
  RL.prompt interface

  pure unit

main :: Eff ( exception :: EXCEPTION
            , console :: CONSOLE
            , readline :: RL.READLINE
            ) Unit
main = runY (usage "$0 -p <player name>") $ map runGame env
  where
  env :: Y GameEnvironment
  env = gameEnvironment <$> yarg "p" ["player"]
                                     (Just "Player name")
                                     (Right "The player name is required")
                                     false
                        <*> flag "d" ["debug"]
                                     (Just "Use debug mode")
                        <*> flag "c" ["cheat"]
                                     (Just "Use cheat mode")
                        
