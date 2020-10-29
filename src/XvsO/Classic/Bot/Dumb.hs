{-# LANGUAGE InstanceSigs #-}

module XvsO.Classic.Bot.Dumb
  ( DumbBot(..)
  ) where

import Data.Maybe  (fromJust)
import Data.Monoid (First(..))

import XvsO.Classic.Game

-- | Bot with simple logic
newtype DumbBot = DumbBot
  { dbReplayCount :: Int  -- ^ Count of games. If less then zero, infinity replay.
  }

instance Player DumbBot where
  makeMove :: Int -> value -> Board value -> DumbBot -> IO (DumbBot, Position)
  makeMove _ _ wBoard bot =
    return (bot, fromJust . getFirst . mconcat $ checkPosition <$> positions)
    where
      positions :: [Position]
      positions =
        [ (0, 0), (0, 1), (0, 2)
        , (1, 0), (1, 1), (1, 2)
        , (2, 0), (2, 1), (2, 2)
        ]

      checkPosition :: Position -> First Position
      checkPosition position = First $
        case unCell $ getCell position wBoard of
          Nothing -> Just position
          Just _  -> Nothing

  startReplay :: DumbBot -> IO (DumbBot, Bool)
  startReplay bot@(DumbBot replayCount)
    | replayCount <  0 = return (bot, True)
    | replayCount == 0 = return (bot, False)
    | otherwise        = return (DumbBot $ pred replayCount, True)

  handleResult :: GameResult -> DumbBot -> IO DumbBot
  handleResult _ = return
