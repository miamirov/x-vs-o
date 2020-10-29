{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

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

import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Data.Aeson    (ToJSON, FromJSON)

import XvsO.Model
import XvsO.Utils

-- | Cell mark of classic game
data XorO = X | O
  deriving (Eq, Show, Generic)

instance ToJSON XorO
instance FromJSON XorO

-- | Type of board with classic marks
type ClassicBoard = Board XorO

-- | Create board of classic size
emptyClassicBoard :: ClassicBoard
emptyClassicBoard = emptyBoard 3 3

-- | Game state of classic mark
type ClassicGameState = GameState XorO

-- | Check classic board on win.
checkBoard :: ClassicBoard -> ClassicGameState
checkBoard wBoard@(Board board)
  | checkWin X = HasWinner X
  | checkWin O = HasWinner O
  | noPlace    = BoardEnd
  | otherwise  = Step
  where
    noPlace :: Bool
    noPlace = not . or $ or <$> (isEmptyCell <$$> board)
    
    checkWin :: XorO -> Bool
    checkWin value =
      or $ and <$> ((cell value ==) . flip getCell wBoard <$$> winMasks)
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

-- | Classic game state
data ClassicGameT playerX playerO = ClassicGameT
  { gPlayerX :: playerX       -- ^ Player of X
  , gPlayerO :: playerO       -- ^ Player of O
  , gStep    :: Int           -- ^ Current step
  , gBoard   :: ClassicBoard  -- ^ Actual board
  }
  deriving (Eq, Show)

-- | Wrapper of classic game state to hide players types
data ClassicGame where
  ClassicGame
    :: (Player playerX, Player playerO, Typeable playerX, Typeable playerO)
    => ClassicGameT playerX playerO
    -> ClassicGame

-- | Create initial state of classic game
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

-- | Do one step of classic game
doStep :: ClassicGame -> IO (ClassicGame, ClassicGameState)
doStep (ClassicGame game) = do
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
      -> IO (ClassicGame, ClassicGameState)
    doStep_ player value updater = do
      (updPlayer, position)
        <- makeMove (gStep game) value (gBoard game) player
      case setCell isEmptyCell position (cell value) (gBoard game) of
        Nothing       -> doStep_ updPlayer value updater
        Just updBoard -> do
          let rGame = updater updPlayer $ ClassicGame $
                game { gStep = succ $ gStep game, gBoard = updBoard }
          return (rGame, checkBoard updBoard)
