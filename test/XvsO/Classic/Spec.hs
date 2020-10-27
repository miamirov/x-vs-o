module XvsO.Classic.Spec
  ( testClassic
  ) where

import Test.Tasty (TestTree, testGroup)

import XvsO.Classic.Bot.Spec (testBot)
import XvsO.Classic.GameTest (testGame)

testClassic :: TestTree
testClassic = testGroup "Test Classic package"
  [ testBot
  , testGame
  ]