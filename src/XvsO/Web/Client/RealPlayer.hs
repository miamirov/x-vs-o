{-# LANGUAGE InstanceSigs #-}

module XvsO.Web.Client.RealPlayer
  ( RealPlayer(..)
  ) where

import Data.Typeable (Typeable)

import XvsO.Model
import XvsO.TUI.Client

-- | Client side of real player
data RealPlayer = RealPlayer

instance Player RealPlayer where
  makeMove
    :: (Show value, Typeable value)
    => Int
    -> value
    -> Board value
    -> RealPlayer
    -> IO (RealPlayer, Position)
  makeMove step value board player = do
    position <- getPosition step value board
    case setCell isEmptyCell position (cell value) board of
      Just _  -> return (player, position)
      Nothing -> putStrLn "This cell isn't empty!" >> makeMove step value board player

  startReplay
    :: RealPlayer
    -> IO (RealPlayer, Bool)
  startReplay player = do
    answer <- askReplay
    return (player, answer) 

  handleResult
    :: GameResult
    -> RealPlayer
    -> IO RealPlayer
  handleResult result player = do
    showResult result
    return player