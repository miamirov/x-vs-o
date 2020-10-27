{-# LANGUAGE TupleSections #-}

module XvsO.Classic.Bot.BotCommonTest
  ( testCommonBot
  ) where

import Control.Monad.State (evalState)
import Data.Typeable       (Typeable)
import Test.Tasty          (TestTree, testGroup)
import Test.Tasty.HUnit    (testCase, (@?=))

import XvsO.Classic.Game

import XvsO.Classic.Bot.ScriptBot
import XvsO.Classic.Bot.BotPlay

testCommonBot :: (Player bot, Typeable bot) => bot -> TestTree
testCommonBot bot = testGroup "Test common bot functionality"
  [ testCase "Bot could do first step" $
      evalState doStep (initClassicGame bot bot) @?= Step
  , testCase "Bot could do second step" $
      evalState (doStep >> doStep) (initClassicGame (ScriptBot [(1, 1)]) bot) @?= Step
  , testCase "Bot could play with script bot as X" $
      runGame
        bot $
        ScriptBot . concat . repeat $
          [(0, ), (1, ), (2, )] <*> [0, 1, 2]
  , testCase "Bot could play with script bot as O" $
      flip runGame
        bot $
        ScriptBot . concat . repeat $
          [(0, ), (1, ), (2, )] <*> [0, 1, 2]
  , testCase "Bot could play with himself" $ runGame bot bot
  ]