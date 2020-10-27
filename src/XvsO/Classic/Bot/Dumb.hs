{-# LANGUAGE InstanceSigs #-}

module XvsO.Classic.Bot.Dumb
  ( DumbBot(..)
  ) where

import Data.Maybe          (fromJust)
import Data.Monoid         (First(..))
import Control.Monad.State (State, get, put)

import XvsO.Classic.Game

newtype DumbBot = DumbBot { dbReplayCount :: Int }

instance Player DumbBot where
  makeMove :: Int -> value -> Board value -> State DumbBot Position
  makeMove _ _ wBoard =
    return . fromJust . getFirst . mconcat $ checkPosition <$> positions
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

  startReplay :: State DumbBot Bool
  startReplay = do
    DumbBot replayCount <- get
    if replayCount <= 0 
    then return False
    else do
      put $ DumbBot (pred replayCount)
      return True

  result :: GameResult -> State DumbBot ()
  result _ = return ()