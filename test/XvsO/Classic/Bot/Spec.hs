module XvsO.Classic.Bot.Spec
  ( testBot
  ) where

import Test.Tasty (TestTree, testGroup)

import XvsO.Classic.Bot.DumbTest (testDumb)

testBot :: TestTree
testBot = testGroup "Test Bot package"
  [ testDumb
  ]