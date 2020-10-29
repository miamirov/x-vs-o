{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module XvsO.Model
  ( Board(..)
  , Cell(..)
  , Position

  , cell
  , cell_
  , emptyBoard

  , getCell
  , isEmptyCell
  , setCell
  , setCell_

  , GameState(..)
  , GameResult(..)

  , state2result

  , Player(..)
  ) where

import Data.Maybe (isNothing, fromJust)

import XvsO.Utils
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Typeable (Typeable)

-- | Type of board cell
newtype Cell value = Cell { unCell :: Maybe value }
  deriving (Eq, Generic)

instance ToJSON value => ToJSON (Cell value)
instance FromJSON value => FromJSON (Cell value)

instance (Show value) => Show (Cell value) where
  show :: Cell value -> String
  show = flip (.) unCell $ \case
    Nothing -> ""
    Just v  -> show v

-- | Wrap value to the cell
cell :: value -> Cell value
cell = Cell . Just

-- | Create empty cell
cell_ :: Cell value
cell_ = Cell Nothing

-- | Check that cell is empty
isEmptyCell :: Cell value -> Bool
isEmptyCell = isNothing . unCell

-- | Type of the game board
newtype Board value = Board { unBoard  :: [[Cell value]] }
  deriving (Eq, Show, Generic)

instance ToJSON value => ToJSON (Board value)
instance FromJSON value => FromJSON (Board value)

-- | Create empty board of given size
emptyBoard :: Int -> Int -> Board value
emptyBoard rows columns = Board $ replicate rows $ replicate columns cell_

-- | Type alias for position
type Position = (Int, Int)

-- | Return cell of the board by position
getCell :: Position -> Board value -> Cell value
getCell (row, column) (Board board) = board !! row !! column

-- | Set if could given value to the given board
setCell :: (Cell value -> Bool) -> Position -> Cell value -> Board value -> Maybe (Board value)
setCell couldSet position@(row, column) newCell wBoard@(Board board)
  | row < 0 && column < 0                    = Nothing
  | not . couldSet $ getCell position wBoard = Nothing
  | otherwise = do
    let line = board !! row
    Just . Board $ setAt row (setAt column newCell line) board

-- | Always could set given value to the given board
setCell_ :: Position -> Cell value -> Board value -> Board value
setCell_ position newCell = fromJust . setCell (const True) position newCell

-- | Type of the game state
data GameState value
  = HasWinner value
  | BoardEnd
  | Step
  deriving (Eq, Show)

-- | Type of game result
data GameResult
  = Win
  | Loose
  | Mirror
  deriving (Generic)

instance ToJSON GameResult
instance FromJSON GameResult

-- | Convert game state to game result fo some value mark
state2result :: (Eq value) => GameState value -> value -> GameResult
state2result Step _ = error "Illegal state for result"
state2result BoardEnd _ = Mirror
state2result (HasWinner winner) player
  | winner == player = Win
  | otherwise        = Loose

-- | Type class of game player
class Player player where
  -- | Make move
  makeMove
    :: (Show value, Typeable value)
    => Int                    -- ^ step number
    -> value                  -- ^ value of mark to set
    -> Board value            -- ^ actual board
    -> player                 -- ^ player to do step
    -> IO (player, Position)  -- ^ (updated player, position to set mark)

  -- | Ask player replay
  startReplay
    :: player             -- ^ player to choose
    -> IO (player, Bool)  -- ^ (updated player, its decision)

  -- | Say to player game result
  handleResult
    :: GameResult  -- ^ Game result
    -> player      -- ^ Player to info
    -> IO player   -- ^ updated player
