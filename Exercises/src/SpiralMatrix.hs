module SpiralMatrix(
  spiral
) where

type Matrix a = [[a]]

spiral :: Matrix a -> [a]
spiral [] = []
spiral matrix = outerMatrix matrix ++ spiral (innerMatrix matrix)

innerMatrix :: Matrix a -> Matrix a
innerMatrix matrix = [middle row | row <- middle matrix]

outerMatrix :: Matrix a -> [a]
outerMatrix matrix = concat [topRow, middle rightColumn, reverse bottomRow, reverse $ middle leftColumn]
  where
    height      = length matrix
    -- Assume the matrix is not jagged.
    width       = length $ head matrix
    topRow      = head matrix
    rightColumn = column (width - 1) matrix
    -- If we only have one row or column, make sure we don't double-count.
    bottomRow   = if height /= 1 then last     matrix else []
    leftColumn  = if width  /= 1 then column 0 matrix else []

middle :: [a] -> [a]
middle []  = []
middle [_] = []
middle xs  = tail . init $ xs

column :: Int -> Matrix a -> [a]
column i = fmap (!! i)