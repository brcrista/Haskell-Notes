{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Either where

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either left right (Left l)  = left l
either left right (Right r) = right r