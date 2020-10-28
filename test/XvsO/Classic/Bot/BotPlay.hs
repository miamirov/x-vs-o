{-# LANGUAGE LambdaCase #-}

module XvsO.Classic.Bot.BotPlay
  ( runGame
  ) where

import Data.Typeable       (Typeable)
import Test.Tasty.HUnit    (Assertion, (@?))

import XvsO.Classic.Game

runGame
  :: (Typeable botX, Typeable botO, Player botX, Player botO)
  => botX -> botO -> Assertion
runGame botX botY = do
  let initialGame = initClassicGame botX botY
  runGame_ initialGame >>= (@? "Too long game")
  where
    runGame_ :: ClassicGame -> IO Bool
    runGame_ game = doStep game >>= \case
      (updGame, Step)        -> runGame_ updGame
      (ClassicGame rGame, _) -> return $ gStep rGame <= 9
