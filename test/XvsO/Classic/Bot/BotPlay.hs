{-# LANGUAGE LambdaCase #-}

module XvsO.Classic.Bot.BotPlay
  ( runGame
  ) where

import Control.Monad.State (State, evalState, get)
import Data.Typeable       (Typeable)
import Test.Tasty.HUnit    (Assertion, (@?))

import XvsO.Classic.Game

runGame
  :: (Typeable botX, Typeable botO, Player botX, Player botO)
  => botX -> botO -> Assertion
runGame botX botY = do
  let initialGame = initClassicGame botX botY
  evalState runGame_ initialGame @? "Too long game"
  where
    runGame_ :: State ClassicGame Bool
    runGame_ = doStep >>= \case
      Step -> runGame_
      _    -> do
        ClassicGame game <- get
        return $ gStep game <= 9
