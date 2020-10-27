module Main
  ( main
  ) where

import Test.Tasty (testGroup, defaultMain)

import XvsO.Spec (testXvsO)

main :: IO ()
main = defaultMain $ testGroup "Test x-vs-o project"
  [ testXvsO
  ]
