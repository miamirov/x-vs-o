module XvsO.ModelTest
  ( testModel
  ) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import XvsO.Model

testModel :: TestTree
testModel = testGroup "Test Model module"
  [ testIsEmptyCell
  , testEmptyBoard
  , testGetCell
  , testSetCell
  ]

testIsEmptyCell :: TestTree
testIsEmptyCell = testGroup "`isEmptyCell` function"
  [ testCase "Empty cell" $ isEmptyCell cell_ @?= True
  , testCase "Non-empty cell" $ isEmptyCell (cell ()) @?= False
  ]

testEmptyBoard :: TestTree
testEmptyBoard = testGroup "`emptyBoard` function"
  [ testCase "Creating empty board" $ do
      let rows = 3
      let columns = 4
      let wBoard@(Board board) = emptyBoard rows columns
      length board @?= rows
      length <$> board @?= replicate rows columns
      forAllCells wBoard $ (@?= True) . isEmptyCell
  ]
  where
    forAllCells :: Board value -> (Cell value -> Assertion) -> Assertion
    forAllCells (Board board) cellAssert = mapM_ cellAssert `mapM_` board

testGetCell :: TestTree
testGetCell = testGroup "`getCell` function"
  [ testCase "Empty board"     $ getCell (1, 1) (makeBoard cell_)     @?= (cell_ :: Cell ())
  , testCase "Non-empty board" $ getCell (1, 1) (makeBoard $ cell ()) @?= cell ()
  ]
  where
    makeBoard :: Cell cell -> Board cell
    makeBoard = Board . replicate 3 . replicate 3

testSetCell :: TestTree
testSetCell = testGroup "`setCell` function"
  [ testCase "Could set" $ do
      let expectedBoard = Board
            [ [cell_, cell_,   cell_]
            , [cell_, cell (), cell_]
            , [cell_, cell_,   cell_]
            ]
      let board = emptyBoard 3 3
      let updatedBoard = setCell isEmptyCell (1, 1) (cell ()) board
      updatedBoard @?= Just expectedBoard
  , testCase "Couldn't set" $ do
      let board = emptyBoard 3 3
      let updatedBoard = setCell (not . isEmptyCell) (1, 1) (cell ()) board
      updatedBoard @?= Nothing
  ]

