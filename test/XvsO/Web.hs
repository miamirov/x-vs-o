module XvsO.Web
  ( ioTestWeb
  ) where

import Test.Hspec (Spec, describe, around, it)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Control.Concurrent.Async (async, waitCatch)

import XvsO.Web.Server
import XvsO.Web.Client
import XvsO.Classic.Bot.Dumb
import XvsO.Classic.Bot.SlowPlayer (SlowPlayer(..))
import Control.Monad (forM_, forM)

ioTestWeb :: Spec
ioTestWeb = describe "Testing web behavior" $ do
  ioTestSingleClient
  ioTestManyClients

ioTestSingleClient :: Spec
ioTestSingleClient = do
  around withUserApp $ do
    describe "Single client connection" $ do
      it "Should work" $ \port -> clientApp_ (DumbBot 0) port

ioTestManyClients :: Spec
ioTestManyClients = do
  around withUserApp $ do
    describe "Legion clients connection" $ do
      it "Should work too" $ \port -> do
        clients <- forM (replicate 500 ()) $ \_ -> async $
          clientApp_ (SlowPlayer 1000 $ DumbBot 0) port
        forM_ clients $ \client -> do
          workResult <- waitCatch client
          case workResult of
            Right _ -> return ()
            Left  e -> ioError $ userError $ show e

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp = testWithApplication serverApp