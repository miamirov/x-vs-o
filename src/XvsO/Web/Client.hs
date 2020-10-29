{-# LANGUAGE TypeApplications #-}

module XvsO.Web.Client
  ( clientApp
  , clientApp_
  ) where

import Servant.Client (BaseUrl(..), Scheme(Http), ClientEnv, runClientM, mkClientEnv)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.WebSockets (Connection, sendClose, receiveData, sendTextData)
import Control.Exception (throwIO)
import Data.Functor ((<&>))
import Control.Monad (when)
import Data.ByteString.Lazy (ByteString)

import XvsO.Web.Api
import XvsO.Web.Client.RealPlayer
import XvsO.Model

type GameClient = GameRoutes (AsClientT IO)

-- | Create game client
mkGameClient :: Manager -> Int -> GameClient
mkGameClient httpManager port =
  genericClientHoist $ \x ->
    runClientM x clientEnv >>= either throwIO pure
  where
    clientEnv :: ClientEnv
    clientEnv =
      mkClientEnv httpManager $
        BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = port
        , baseUrlPath = ""
        }

-- | Create client application with default player
clientApp :: Int -> IO ()
clientApp = clientApp_ RealPlayer

-- | Create client application
clientApp_ :: Player player => player -> Int -> IO ()
clientApp_ initialPlayer port = do
  httpManager <- newManager defaultManagerSettings
  gameHost (mkGameClient httpManager port) $ \connection -> do
    putStrLn "You connect successefuly"
    runGame initialPlayer connection
    sendClose connection $ mempty @ByteString
  where
    runGame :: Player player => player -> Connection -> IO ()
    runGame player connection = do
      resultOrMoveData <- receiveData connection <&> decodeResultOrMoveData
      case resultOrMoveData of
        Nothing -> sendTextData connection (encodePosition (-1, -1)) >> runGame player connection
        Just (Left result) -> do
          (updPlayer, wantReplay) <- startReplay =<< handleResult result player
          sendTextData connection $ encodeBool wantReplay
          when wantReplay $ runGame updPlayer connection
        Just (Right md) -> do
          (updPlayer, position) <- makeMove (mdStep md) (mdValue md) (mdBoard md) player
          sendTextData connection $ encodePosition position
          runGame updPlayer connection
