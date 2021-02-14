import Data.List (transpose)
import Data.Maybe (listToMaybe, mapMaybe)

data Player = PlayerX | PlayerO deriving (Eq, Show)
data Square = X | O | Empty deriving (Eq, Show)

type Board = [[Square]]

evaluateBoard :: Board -> Maybe Player
evaluateBoard board = listToMaybe $ mapMaybe allInARow $ directions board

-- "Row" here means any direction (row, column, diagonal).
allInARow :: [Square] -> Maybe Player
allInARow row
  | all (== X) row = Just PlayerX
  | all (== O) row = Just PlayerO
  | otherwise      = Nothing

directions :: Board -> [[Square]]
directions board = concat $ [rows, columns, diagonals] <*> [board]

rows :: Board -> [[Square]]
rows = id

columns :: Board -> [[Square]]
columns = transpose

diagonals :: Board -> [[Square]]
diagonals board =
  [
    zipWith (!!) <*> indices,
    zipWith (!!) <*> reverse . indices
  ] <*> [board]
  where indices = zipWith const [0 ..]

{-
Tests:
evaluateBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] == Nothing
evaluateBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [X, X, X]]             == Just PlayerX
evaluateBoard [[Empty, Empty, X], [Empty, Empty, X], [X, Empty, X]]                 == Just PlayerX
evaluateBoard [[O, Empty, X], [X, O, X], [X, Empty, O]]                             == Just PlayerO
evaluateBoard [[O, Empty, X], [Empty, X, X], [X, Empty, O]]                         == Just PlayerX
-}