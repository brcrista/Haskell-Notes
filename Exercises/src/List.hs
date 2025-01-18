module List
where

data List a = End | List a (List a)
  deriving (Eq)

instance Show a => Show (List a) where
  show list = "[" ++ go list ++ "]"
    where
      go End = ""
      go (List x End) = show x
      go (List x xs)  = show x ++ "," ++ go xs

instance Semigroup (List a) where
  list1 <> End   = list1
  End   <> list2 = list2
  (List x xs) <> list2 = List x (xs <> list2)

instance Monoid (List a) where
  mempty = End

instance Functor List where
  fmap _ End = End
  fmap f (List x xs) = List (f x) (fmap f xs)

instance Foldable List where
  foldr _ acc End = acc
  foldr f acc (List x xs) = f x (foldr f acc xs)

idx :: List a -> Int -> a
idx End _ = error "index too large"
idx (List x xs) n
  | n  < 0    = error "negative index"
  | n == 0    = x
  | otherwise = idx xs (n - 1)

singleton :: a -> List a
singleton x = List x End

reverse' :: List a -> List a
reverse' End = End
reverse' (List x xs) = reverse' xs <> List x End