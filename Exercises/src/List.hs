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

instance Functor List where
  fmap _ End = End
  fmap f (List x xs) = List (f x) (fmap f xs)

instance Foldable List where
  foldr _ acc End = acc
  foldr f acc (List x xs) = f x (foldr f acc xs)
