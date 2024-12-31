module Tests.FallingWater where

import FallingWater
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

test_FallingWater = caseGroup "Falling water"
  [
    fallingWater [] @?= [],
    fallingWater [0] @?= [0],
    fallingWater [0, 1, 0] @?= [0, 0, 0],
    fallingWater [0, 1, 2] @?= [0, 0, 0],
    fallingWater [1] @?= [0],
    fallingWater [1, 0, 1] @?= [0, 1, 0],
    fallingWater [2, 0, 1] @?= [0, 1, 0],
    fallingWater [1, 0, 0, 2] @?= [0, 1, 1, 0],
    fallingWater [0, 3, 0, 1, 4, 2, 5] @?= [0, 0, 3, 2, 0, 2, 0],
    fallingWater [0, 5, 0, 1, 4, 2, 5] @?= [0, 0, 5, 4, 1, 3, 0]
  ]
