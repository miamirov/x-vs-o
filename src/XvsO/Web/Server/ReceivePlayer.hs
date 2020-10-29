{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module XvsO.Web.Server.ReceivePlayer
  ( ReceivePlayer(..)
  ) where

import Data.Typeable      (eqT, (:~:)(Refl), Typeable)
import Data.Functor       ((<&>))
import Network.WebSockets (Connection, sendTextData, receiveData)

import XvsO.Classic.Game
import XvsO.Web.Api

-- | Server side of real player
newtype ReceivePlayer = ReceivePlayer { wpConnection :: Connection }

instance Player ReceivePlayer where
  makeMove
    :: (Show value, Typeable value)
    => Int
    -> value
    -> Board value
    -> ReceivePlayer
    -> IO (ReceivePlayer, Position)
  makeMove s (v :: vT) b p =
    case eqT @vT @XorO of
      Nothing -> error "web player undestand x or o only"
      Just Refl -> do
        let moveData = MoveData
              { mdStep = s
              , mdValue = v
              , mdBoard = b
              }
        sendTextData (wpConnection p) $ encodeResultOrMoveData (Right moveData)
        mbPosition <- receiveData (wpConnection p) <&> decodePosition
        case mbPosition of
          Nothing       -> makeMove s v b p
          Just position -> return (p, position)

  startReplay
    :: ReceivePlayer
    -> IO (ReceivePlayer, Bool)
  startReplay p = do
    mbWantReplay <- receiveData (wpConnection p) <&> decodeBool
    case mbWantReplay of
      Nothing         -> startReplay p
      Just wantReplay -> return (p, wantReplay)

  handleResult
    :: GameResult
    -> ReceivePlayer
    -> IO ReceivePlayer
  handleResult result p= do
    sendTextData (wpConnection p) $ encodeResultOrMoveData (Left result)
    return p
