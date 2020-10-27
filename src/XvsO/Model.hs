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
import Control.Monad.State (State)

newtype Cell value = Cell { unCell :: Maybe value }
  deriving (Eq, Show)

cell :: value -> Cell value
cell = Cell . Just

cell_ :: Cell value
cell_ = Cell Nothing

isEmptyCell :: Cell value -> Bool
isEmptyCell = isNothing . unCell

newtype Board value = Board { unBoard  :: [[Cell value]] }
  deriving (Eq, Show)

emptyBoard :: Int -> Int -> Board value
emptyBoard rows columns = Board $ replicate rows $ replicate columns cell_

type Position = (Int, Int)

getCell :: Position -> Board value -> Cell value
getCell (row, column) (Board board) = board !! row !! column

setCell :: (Cell value -> Bool) -> Position -> Cell value -> Board value -> Maybe (Board value)
setCell couldSet position@(row, column) newCell wBoard@(Board board)
  | not . couldSet $ getCell position wBoard = Nothing
  | otherwise = do
    let line = board !! row
    Just . Board $ setAt row (setAt column newCell line) board
  where
    setAt :: Int -> a -> [a] -> [a]
    setAt i a ls
      | i < 0 = ls
      | otherwise = go i ls
      where
        go 0 (_:xs) = a : xs
        go n (x:xs) = x : go (n-1) xs
        go _ []     = []

setCell_ :: Position -> Cell value -> Board value -> Board value
setCell_ position newCell = fromJust . setCell (const True) position newCell

data GameState value
  = HasWinner value
  | BoardEnd
  | Step
  deriving (Eq, Show)

data GameResult
  = Win
  | Loose
  | Mirror

state2result :: (Eq value) => GameState value -> value -> GameResult
state2result Step _ = error "Illegal state for result"
state2result BoardEnd _ = Mirror
state2result (HasWinner winner) player
  | winner == player = Win
  | otherwise        = Loose

class Player player where
  makeMove
    :: Int
    -> value
    -> Board value
    -> State player Position

  startReplay
    :: State player Bool

  result
    :: GameResult
    -> State player ()
