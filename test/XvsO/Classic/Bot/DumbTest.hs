module XvsO.Classic.Bot.DumbTest
  ( testDumb
  ) where

import Test.Tasty (TestTree, testGroup)

import XvsO.Classic.Bot.Dumb

import XvsO.Classic.Bot.BotCommonTest

testDumb :: TestTree
testDumb = testGroup "Test Dumb package"
  [ testCommonBot (DumbBot 0)
  ]


