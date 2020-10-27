module XvsO.Classic.GameTest
  ( testGame
  ) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import XvsO.Classic.Game

testGame :: TestTree
testGame = testGroup "Test Game module"
  [ testCheckBoard
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
      foldr ($) emptyClassicBoard . (flip setCell_ (cell value) <$>) <$> winMasks
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
    mirrorSituation = Board $ (cell <$>) <$>
      [ [O, X, X]
      , [X, X, O]
      , [O, O, X]
      ]
    
    winOnLastStep :: ClassicBoard
    winOnLastStep = Board $ (cell <$>) <$>
      [ [X, O, X]
      , [X, X, O]
      , [O, O, X]
      ]