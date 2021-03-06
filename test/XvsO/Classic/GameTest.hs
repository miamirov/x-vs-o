{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module XvsO.Classic.GameTest
  ( testGame
  ) where

import Data.Typeable       (eqT, (:~:)(Refl))
import Test.Tasty          (TestTree, testGroup)
import Test.Tasty.HUnit    (Assertion, testCase, (@?=), assertFailure)

import XvsO.Classic.Game
import XvsO.Utils

import XvsO.Classic.Bot.ScriptBot

testGame :: TestTree
testGame = testGroup "Test Game module"
  [ testCheckBoard
  , testDoStep
  ]

testCheckBoard :: TestTree
testCheckBoard = testGroup "`checkBoard` function"
  [ testCase "Mirror situation" $ checkBoard mirrorSituation @?= BoardEnd
  , testCase "Win X situation"  $ mapM_ (@?= HasWinner X) $ checkBoard <$> winBoards X
  , testCase "Win O situation"  $ mapM_ (@?= HasWinner O) $ checkBoard <$> winBoards O
  , testCase "Win on last step" $ checkBoard winOnLastStep   @?= HasWinner X
  ]
  where
    winBoards :: XorO -> [ClassicBoard]
    winBoards value =
      foldr ($) emptyClassicBoard <$> (flip setCell_ (cell value) <$$> winMasks)
      where
        winMasks :: [[(Int, Int)]]
        winMasks =
          [ [ (0, 0), (0, 1), (0, 2) ]
          , [ (1, 0), (1, 1), (1, 2) ]
          , [ (2, 0), (2, 1), (2, 2) ]
    
          , [ (0, 0), (1, 0), (2, 0) ]
          , [ (0, 1), (1, 1), (2, 1) ]
          , [ (0, 2), (1, 2), (2, 2) ]
          
          , [ (0, 0), (1, 1), (2, 2) ]
          , [ (0, 2), (1, 1), (2, 0) ]
          ]

    mirrorSituation :: ClassicBoard
    mirrorSituation = Board $ cell <$$>
      [ [O, X, X]
      , [X, X, O]
      , [O, O, X]
      ]
    
    winOnLastStep :: ClassicBoard
    winOnLastStep = Board $ cell <$$>
      [ [X, O, X]
      , [X, X, O]
      , [O, O, X]
      ]

testDoStep :: TestTree
testDoStep = testGroup "`doStep` function"
  [ testCase "Do correct step" $
      testSteps
        (ScriptBot [(1, 1)])
        emptyScriptPlayer
        Step
        ClassicGameT
          { gPlayerX = emptyScriptPlayer
          , gPlayerO = emptyScriptPlayer
          , gStep    = 1
          , gBoard   = setCell_ (1, 1) (cell X) emptyClassicBoard
          }
  , testCase "Do incorrect step" $
      testSteps
        (ScriptBot [(1, 1)])
        (ScriptBot [(1, 1), (1, 1), (1, 1), (0, 0)])
        Step
        ClassicGameT
          { gPlayerX = emptyScriptPlayer
          , gPlayerO = emptyScriptPlayer
          , gStep    = 2
          , gBoard   = setCell_ (1, 1) (cell X) $
                       setCell_ (0, 0) (cell O) emptyClassicBoard
          }
  , testCase "Win X" $
      testSteps
        (ScriptBot [(0, 0), (0, 1), (0, 2)])
        (ScriptBot [(1, 0), (1, 1)])
        (HasWinner X)
        ClassicGameT
          { gPlayerX = emptyScriptPlayer
          , gPlayerO = emptyScriptPlayer
          , gStep    = 5
          , gBoard   = Board
              [ [ cell X, cell X, cell X ]
              , [ cell O, cell O, cell_  ]
              , [ cell_,  cell_,  cell_  ]
              ]
          }
  , testCase "Win O" $
      testSteps
        (ScriptBot [(0, 0), (0, 1), (2, 0)])
        (ScriptBot [(1, 0), (1, 1), (1, 2)])
        (HasWinner O)
        ClassicGameT
          { gPlayerX = emptyScriptPlayer
          , gPlayerO = emptyScriptPlayer
          , gStep    = 6
          , gBoard   = Board
              [ [ cell X, cell X, cell_  ]
              , [ cell O, cell O, cell O ]
              , [ cell X, cell_,  cell_  ]
              ]
          }
  , testCase "Mirror" $
      testSteps
        (ScriptBot [(0, 1), (0, 2), (1, 0), (1, 1), (2, 2)])
        (ScriptBot [(0, 0), (1, 2), (2, 0), (2, 1)])
        BoardEnd
        ClassicGameT
          { gPlayerX = emptyScriptPlayer
          , gPlayerO = emptyScriptPlayer
          , gStep    = 9
          , gBoard   = Board $ cell <$$>
              [ [O, X, X]
              , [X, X, O]
              , [O, O, X]
              ]
          }
  ]
  where
    testSteps
      :: ScriptBot
      -> ScriptBot
      -> ClassicGameState
      -> ClassicGameT ScriptBot ScriptBot
      -> Assertion
    testSteps
      playerX
      playerO
      expectedResult
      expectedGame
        =
      do
        let initialGame = initClassicGame playerX playerO
        (ClassicGame (game :: ClassicGameT tPlayerX tPlayerO), gameState)
          <- doStep_ (gStep expectedGame) initialGame
        case (eqT @tPlayerX @ScriptBot, eqT @tPlayerO @ScriptBot) of
          (Just Refl, Just Refl) -> do
            gameState @?= expectedResult
            game @?= expectedGame
          _ -> assertFailure "Player change type!"
    
    doStep_ :: Int -> ClassicGame -> IO (ClassicGame, ClassicGameState)
    doStep_ 1 game = doStep game
    doStep_ i game = doStep game >>= doStep_ (pred i) . fst