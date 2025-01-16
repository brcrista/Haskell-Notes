module List
where

data List a = Empty | List a (List a)
  deriving (Eq)

instance Show a => Show (List a) where
  show Empty = "[]"
  show (List x xs) = show x ++ ":" ++ show xs

instance Functor List where
  fmap _ Empty = Empty
  fmap f (List x xs) = List (f x) (fmap f xs)

instance Foldable List where
  foldr _ acc Empty = acc
  foldr f acc (List x xs) = f x (foldr f acc xs)
