{-# LANGUAGE InstanceSigs #-}

module XvsO.Classic.ScriptPlayer
  ( ScriptPlayer(..)

  , emptyScriptPlayer
  ) where

import Control.Monad.State (State, put, get)

import XvsO.Model

newtype ScriptPlayer = ScriptPlayer { spData :: [Position] }
  deriving (Eq, Show)

instance Player ScriptPlayer where
  makeMove
    :: Int
    -> value
    -> Board value
    -> State ScriptPlayer Position
  makeMove step _ _ = do
    ScriptPlayer data_ <- get
    case data_ of
      [] -> error $ "Step isn't exists: " ++ show step
      position:tailData -> do
         put $ ScriptPlayer tailData
         return position

  startReplay
    :: State ScriptPlayer Bool
  startReplay = do
    ScriptPlayer data_ <- get
    return $ not $ null data_

  result
    :: GameResult
    -> State ScriptPlayer ()
  result _ = return ()

emptyScriptPlayer :: ScriptPlayer
emptyScriptPlayer = ScriptPlayer mempty
