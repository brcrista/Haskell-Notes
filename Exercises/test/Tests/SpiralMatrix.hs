module Tests.SpiralMatrix where

import SpiralMatrix
import Test.Tasty.HUnit
import Tests.Helpers

test_emptyMatrix = testCase "Empty matrix" $ (spiral [] :: [[Int]]) @?= []
test_singletonMatrix = testCase "Singleton matrix" $ spiral [[1]] @?= [1]

test_squareMatrix = testCase "Square matrix" $ spiral matrix @?= expected
  where
    expected = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    matrix =
      [
        [1, 2, 3],
        [8, 9, 4],
        [7, 6, 5]
      ]

test_rectangularMatrix = caseGroup "Rectangular matrix"
  [
    spiral [[1, 2]] @?= [1, 2],
    spiral [[1], [2], [3]] @?= [1, 2, 3]
  ]
