import Data.List (transpose)
import Data.Maybe (listToMaybe, mapMaybe)

data Player = PlayerX | PlayerO deriving (Eq, Show)
data Square = X | O | Empty deriving (Eq, Show)

type Board = [[Square]]

evaluateBoard :: Board -> Maybe Player
evaluateBoard board = listToMaybe $ mapMaybe threeInARow $ directions board

threeInARow :: [Square] -> Maybe Player
threeInARow [X, X, X] = Just PlayerX
threeInARow [O, O, O] = Just PlayerO
threeInARow _         = Nothing

directions :: Board -> [[Square]]
directions board = concat $ [rows, columns, diagonals] <*> [board]

rows :: Board -> [[Square]]
rows = id

columns :: Board -> [[Square]]
columns = transpose

diagonals :: Board -> [[Square]]
diagonals board =
  [
    zipWith (!!) board [0, 1, 2],
    zipWith (!!) board [2, 1, 0]
  ]

{-
Tests:
evaluateBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] == Nothing
evaluateBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [X, X, X]]             == Just PlayerX
evaluateBoard [[Empty, Empty, X], [Empty, Empty, X], [X, Empty, X]]                 == Just PlayerX
evaluateBoard [[O, Empty, X], [X, O, X], [X, Empty, O]]                             == Just PlayerO
-}