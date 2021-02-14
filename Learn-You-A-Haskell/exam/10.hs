import Control.Applicative

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
threeInARow _         = Nothing

directions :: Board -> [[Square]]
directions board = concat $ [rows, columns, diagonals] <*> [board]

rows :: Board -> [[Square]]
rows = id

columns :: Board -> [[Square]]
columns board =
  [
    (!!) <$> board <*> [0],
    (!!) <$> board <*> [1],
    (!!) <$> board <*> [2]
  ]

diagonals :: Board -> [[Square]]
diagonals board =
  [
    getZipList $ (!!) <$> ZipList board <*> ZipList [0, 1, 2],
    getZipList $ (!!) <$> ZipList board <*> ZipList [2, 1, 0]
  ]

{-
Tests:
evaluateBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] @?= Nothing
evaluateBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [X, X, X]]             @?= Just PlayerX
evaluateBoard [[Empty, Empty, X], [Empty, Empty, X], [X, Empty, X]]                 @?= Just PlayerX
evaluateBoard [[O, Empty, X], [X, O, X], [X, Empty, O]]                             @?= Just PlayerO
-}