module Main 
  ( main
  ) where

import XvsO.Web.Client (runClient)
import XvsO.TUI.Client
import XvsO.Model

main :: IO ()
main = do
  p <- getPosition 0 () (emptyBoard 4 4)
  print p
  showResult Win
  r <- askReplay
  print r
  
  -- runClient
