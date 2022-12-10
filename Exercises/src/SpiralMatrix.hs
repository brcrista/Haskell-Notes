module SpiralMatrix(
  spiral
) where

type Matrix a = [[a]]

spiral :: Matrix a -> [a]
spiral [] = []
spiral matrix = outerMatrix ++ spiral innerMatrix
  where
    -- Assume the matrix is not jagged.
    width       = length $ head matrix
    height      = length matrix
    topRow      = head matrix
    rightColumn = column (width - 1) matrix
    -- If we only have one row or column, make sure we don't double-count.
    bottomRow   = if height /= 1 then last     matrix else []
    leftColumn  = if width  /= 1 then column 0 matrix else []
    -- Just take the middle elements from the columns so they're not double-counted.
    outerMatrix = concat [topRow, middle rightColumn, reverse bottomRow, reverse $ middle leftColumn]
    innerMatrix = [middle row | row <- middle matrix]

middle :: [a] -> [a]
middle []  = []
middle [_] = []
middle xs  = tail . init $ xs

column :: Int -> Matrix a -> [a]
column i = map (!! i)