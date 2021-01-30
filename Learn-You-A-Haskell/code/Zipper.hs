newtype Zipper a = Zipper { focus :: ([a], [a]) } deriving Show

start :: [a] -> Zipper a
start xs = Zipper ([], xs)

next :: Zipper a -> Zipper a
next (Zipper (behind, [])) = error "end of list"
next (Zipper (behind, x : xs)) = Zipper (x : behind, xs)

prev :: Zipper a -> Zipper a
prev (Zipper ([], xs)) = error "beginning of list"
prev (Zipper (x : xs, ahead)) = Zipper (xs, x : ahead)