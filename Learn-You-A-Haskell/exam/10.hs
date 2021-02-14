data Player = PlayerX | PlayerO deriving Show
data Square = X | O | Empty deriving Show

type Board = [[Square]]

evaluateBoard :: Board -> Maybe Player
evaluateBoard board = first $ map threeInARow (directions board)

first :: [Maybe a] -> Maybe a
first ((Just x) : xs) = Just x
first (Nothing  : xs) = first xs
first []              = Nothing

threeInARow :: [Square] -> Maybe Player
threeInARow [X, X, X] = Just PlayerX
threeInARow [O, O, O] = Just PlayerO
threeInARow _ = Nothing

directions :: Board -> [[Square]]
directions board = concat [rows board, columns board, diagonals board]

rows :: Board -> [[Square]]
rows board = [board !! 0, board !! 1, board !! 2]

columns :: Board -> [[Square]]
columns board =
  [
    [(board !! 0) !! 0, (board !! 1) !! 0, (board !! 2) !! 0],
    [(board !! 0) !! 1, (board !! 1) !! 1, (board !! 2) !! 1],
    [(board !! 0) !! 2, (board !! 1) !! 2, (board !! 2) !! 2]
  ]

diagonals :: Board -> [[Square]]
diagonals board =
  [
    [(board !! 0) !! 0, (board !! 1) !! 1, (board !! 2) !! 2],
    [(board !! 0) !! 2, (board !! 1) !! 1, (board !! 2) !! 0]
  ]