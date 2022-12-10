module FallingWater(
  fallingWater
) where

fallingWater :: (Num a, Ord a) => [a] -> [a]
fallingWater xs = zipWith3 waterDepth maxLeft xs maxRight
  where
    maxLeft  = scanl max 0 xs
    maxRight = scanr max 0 xs

waterDepth left x right = max 0 (waterHeight - x)
  where waterHeight = min left right