{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module XvsO.Classic.Bot.SlowPlayer
  ( SlowPlayer(..)
  ) where

import XvsO.Model
import Data.Typeable (Typeable)
import Control.Concurrent (threadDelay)
import Data.Bifunctor (first)

data SlowPlayer player = SlowPlayer Int player

instance Player player => Player (SlowPlayer player) where
  makeMove
    :: (Show value, Typeable value)
    => Int
    -> value
    -> Board value
    -> SlowPlayer player
    -> IO (SlowPlayer player, Position)
  makeMove step value board (SlowPlayer timout player) = do
    threadDelay timout
    first (SlowPlayer timout) <$> makeMove step value board player

  startReplay
    :: SlowPlayer player
    -> IO (SlowPlayer player, Bool)
  startReplay (SlowPlayer timout player) = do
    threadDelay timout
    first (SlowPlayer timout) <$> startReplay player

  handleResult
    :: GameResult
    -> SlowPlayer player
    -> IO (SlowPlayer player)
  handleResult result (SlowPlayer timout player) = do
    threadDelay timout
    SlowPlayer timout <$> handleResult result player