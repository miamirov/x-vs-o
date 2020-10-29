{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module XvsO.Web.Server
  ( runServer
  , serverApp
  ) where

import Network.Wai.Handler.Warp (run)

import XvsO.Web.Api
import XvsO.Classic.Game
import XvsO.Classic.Bot.Dumb
import XvsO.Web.Server.ReceivePlayer

import Servant.Server.Generic (AsServer, genericServe)
import Servant.Server (Application, Handler)
import Network.WebSockets (Connection, PendingConnection, acceptRequest, withPingThread)

import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Control.Monad (when)


type GameServer = GameRoutes AsServer

-- | Create server application
serverApp :: IO Application
serverApp = return $ genericServe mkGameServer

-- | Create game server
mkGameServer :: GameServer
mkGameServer = GameRoutes { gameHost }
  where
    gameHost :: PendingConnection -> Handler ()
    gameHost pendingConn = liftIO $ do
      start =<< acceptRequest pendingConn
      where
        start :: Connection -> IO ()
        start connection =
          withPingThread connection 30 (return ()) $
            flip finally (return ()) $ do
              runGame $ initClassicGame (ReceivePlayer connection) (DumbBot (-1))

        runGame :: ClassicGame -> IO ()
        runGame initialGame = do
          (ClassicGame game, result) <- processGame initialGame
          playerX <- handleResult (state2result result X) $ gPlayerX game
          playerO <- handleResult (state2result result O) $ gPlayerO game
          (playerX_, xReplay) <- startReplay playerX
          (playerO_, oReplay) <- startReplay playerO
          when (xReplay && oReplay)
            $ runGame $ initClassicGame playerO_ playerX_
          where
            processGame :: ClassicGame -> IO (ClassicGame, ClassicGameState)
            processGame game = doStep game >>= \case
              (updGame, Step)   -> processGame updGame
              (updGame, result) -> return     (updGame, result)

-- | Run game server
runServer :: Int -> IO ()
runServer port = run port =<< serverApp
