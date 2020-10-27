{-# LANGUAGE InstanceSigs #-}

module XvsO.Classic.Bot.ScriptBot
  ( ScriptBot(..)

  , emptyScriptPlayer
  ) where

import Control.Monad.State (State, put, get)

import XvsO.Model

newtype ScriptBot = ScriptBot { spData :: [Position] }
  deriving (Eq, Show)

instance Player ScriptBot where
  makeMove
    :: Int
    -> value
    -> Board value
    -> State ScriptBot Position
  makeMove step _ _ = do
    ScriptBot data_ <- get
    case data_ of
      [] -> error $ "Step isn't exists: " ++ show step
      position:tailData -> do
         put $ ScriptBot tailData
         return position

  startReplay
    :: State ScriptBot Bool
  startReplay = do
    ScriptBot data_ <- get
    return $ not $ null data_

  handleResult
    :: GameResult
    -> State ScriptBot ()
  handleResult _ = return ()

emptyScriptPlayer :: ScriptBot
emptyScriptPlayer = ScriptBot mempty
