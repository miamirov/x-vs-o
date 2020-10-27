module XvsO.Classic.Game
  ( module XvsO.Model

  , XorO(..)
  , ClassicBoard
  , ClassicGameState

  , emptyClassicBoard

  , checkBoard
  ) where

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
    checkWin value = or $ and . ((cell value ==) . flip getCell wBoard <$>) <$> winMasks
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
