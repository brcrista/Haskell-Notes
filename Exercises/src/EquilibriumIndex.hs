module EquilibriumIndex(
  equilibriumIndex
) where

import Data.List (findIndex)

equilibriumIndex :: Integral a => [a] -> Maybe Int
equilibriumIndex xs = findIndex id $ zipWith (==) leftSums rightSums
  where
    leftSums  = scanl (+) 0 xs
    rightSums = tail $ scanr (+) 0 xs