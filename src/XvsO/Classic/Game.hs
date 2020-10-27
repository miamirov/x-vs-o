{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XvsO.Classic.Game
  ( module XvsO.Model

  , XorO(..)
  , ClassicBoard
  , ClassicGameState

  , emptyClassicBoard

  , checkBoard
  
  , ClassicGame(..)
  , ClassicGameT(..)

  , initClassicGame
  , doStep
  ) where

import Control.Monad.State (State, get, runState, put)
import Data.Typeable (Typeable)

import XvsO.Model

data XorO = X | O
  deriving (Eq, Show)

type ClassicBoard = Board XorO

emptyClassicBoard :: ClassicBoard
emptyClassicBoard = emptyBoard 3 3

type ClassicGameState = GameState XorO

checkBoard :: ClassicBoard -> ClassicGameState
checkBoard wBoard@(Board board)
  | checkWin X = HasWinner X
  | checkWin O = HasWinner O
  | noPlace    = BoardEnd
  | otherwise  = Step
  where
    noPlace :: Bool
    noPlace = not . or $ or . (isEmptyCell <$>) <$> board
    
    checkWin :: XorO -> Bool
    checkWin value =
      or $ and . ((cell value ==) . flip getCell wBoard <$>) <$> winMasks
      where
        winMasks :: [[Position]]
        winMasks =
          [ [ (0, 0), (0, 1), (0, 2) ]
          , [ (1, 0), (1, 1), (1, 2) ]
          , [ (2, 0), (2, 1), (2, 2) ]
    
          , [ (0, 0), (1, 0), (2, 0) ]
          , [ (0, 1), (1, 1), (2, 1) ]
          , [ (0, 2), (1, 2), (2, 2) ]
          
          , [ (0, 0), (1, 1), (2, 2) ]
          , [ (0, 2), (1, 1), (2, 0) ]
          ]

data ClassicGameT playerX playerO = ClassicGameT
  { gPlayerX :: playerX
  , gPlayerO :: playerO
  , gStep    :: Int
  , gBoard   :: ClassicBoard
  }
  deriving (Eq, Show)

data ClassicGame where
  ClassicGame
    :: (Player playerX, Player playerO, Typeable playerX, Typeable playerO)
    => ClassicGameT playerX playerO
    -> ClassicGame

initClassicGame
  :: (Player playerX, Player playerO, Typeable playerX, Typeable playerO)
  => playerX -> playerO -> ClassicGame
initClassicGame player1 player2 =
  ClassicGame $ ClassicGameT
    { gPlayerX = player1
    , gPlayerO = player2
    , gStep    = 0
    , gBoard   = emptyClassicBoard
    }

doStep :: State ClassicGame ClassicGameState
doStep = do
  ClassicGame game <- get
  if even $ gStep game
  then doStep_ (gPlayerX game) X $
         \p (ClassicGame b) -> ClassicGame $ b { gPlayerX = p }
  else doStep_ (gPlayerO game) O $
         \p (ClassicGame b) -> ClassicGame $ b { gPlayerO = p }
  where
    doStep_
      :: (Player player)
      => player
      -> XorO
      -> (player -> ClassicGame -> ClassicGame)
      -> State ClassicGame ClassicGameState
    doStep_ player value updater = do
      ClassicGame game <- get
      let (position, updPlayer) =
            runState (makeMove (gStep game) value (gBoard game)) player
      case setCell isEmptyCell position (cell value) (gBoard game) of
        Nothing       -> doStep_ updPlayer value updater
        Just updBoard -> do
          put . updater updPlayer . ClassicGame $ 
            game { gStep = succ $ gStep game, gBoard = updBoard }
          return $ checkBoard updBoard
