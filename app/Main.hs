{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main 
  ( main
  ) where

import XvsO.Web.Api (clientApp)
import XvsO.Web.Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    ["server", port] -> runServer (read @Int port)
    ["client", port] -> clientApp (read @Int port)
    args -> error $ "Invalid arguments" <> show args
