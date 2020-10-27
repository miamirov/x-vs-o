module XvsO.Spec
  ( testXvsO
  ) where

import Test.Tasty (TestTree, testGroup)

import XvsO.Classic.Spec (testClassic)
import XvsO.ModelTest    (testModel)

testXvsO :: TestTree
testXvsO = testGroup "Test XvsO package"
  [ testClassic
  , testModel
  ]