module XvsO.Spec
  ( testXvsO
  , ioTestXvsO
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Hspec (Spec, describe)

import XvsO.Classic.Spec (testClassic)
import XvsO.ModelTest    (testModel)
import XvsO.Web          (ioTestWeb)

testXvsO :: TestTree
testXvsO = testGroup "Test XvsO package"
  [ testClassic
  , testModel
  ]

ioTestXvsO :: Spec
ioTestXvsO = describe "Test XvsO package" $ do
  ioTestWeb