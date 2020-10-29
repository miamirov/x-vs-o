module Main
  ( main
  ) where

import Test.Tasty (testGroup, defaultMain)

import XvsO.Spec (testXvsO, ioTestXvsO)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  ioTests <- testSpec "Spec tests" ioTestXvsO
  defaultMain $
    testGroup "Test x-vs-o project" $ ioTests :
      [ testXvsO
      ]
