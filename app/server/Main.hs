module Main 
  ( main
  ) where

import XvsO.Web.Server (runServer)

main :: IO ()
main = runServer 8080
