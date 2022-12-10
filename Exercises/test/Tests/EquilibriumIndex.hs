module Tests.EquilibriumIndex where

import EquilibriumIndex
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

test_emptyList = caseGroup "Empty list"
  [
    equilibriumIndex [] @?= Nothing
  ]

test_nonnegativeIntegers = caseGroup "Nonnegative integers"
  [
    equilibriumIndex [3] @?= Just 0,
    equilibriumIndex [1, 3, 1] @?= Just 1,
    equilibriumIndex [1, 3, 4, 0] @?= Nothing,
    equilibriumIndex [1, 3, 0, 4] @?= Just 2,
    equilibriumIndex [0, 1] @?= Just 1
  ]

test_negativeIntegers = caseGroup "Negative integers"
  [
    equilibriumIndex [-1, -1, -1] @?= Just 1,
    equilibriumIndex [-1, 1, 0] @?= Just 2,
    equilibriumIndex [-1, 1, 0] @?= Just 2,
    equilibriumIndex [-1, 99, -100] @?= Nothing,
    equilibriumIndex [-1, 100000, 99, -100] @?= Just 1
  ]

test_multipleEquilibriumIndices = caseGroup "Multiple solutions"
  [
    equilibriumIndex [0, 0] @?= Just 0, -- also 1
    equilibriumIndex [-1, 3, -4, 5, 1, -6, 2, 1] @?= Just 1 -- also 3, 7
  ]
