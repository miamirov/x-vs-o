{-# LANGUAGE InstanceSigs #-}

module XvsO.Classic.Bot.ScriptBot
  ( ScriptBot(..)

  , emptyScriptPlayer
  ) where

import XvsO.Model

newtype ScriptBot = ScriptBot { spData :: [Position] }
  deriving (Eq, Show)

instance Player ScriptBot where
  makeMove
    :: Int
    -> value
    -> Board value
    -> ScriptBot
    -> IO (ScriptBot, Position)
  makeMove step _ _ (ScriptBot data_) = do
    case data_ of
      []                -> ioError . userError $ "Step isn't exists: " ++ show step
      position:tailData -> return (ScriptBot tailData, position)

  startReplay :: ScriptBot -> IO (ScriptBot, Bool)
  startReplay bot@(ScriptBot data_) =
    return (bot, not $ null data_)

  handleResult
    :: GameResult
    -> ScriptBot
    -> IO ScriptBot
  handleResult _ = return

emptyScriptPlayer :: ScriptBot
emptyScriptPlayer = ScriptBot mempty
