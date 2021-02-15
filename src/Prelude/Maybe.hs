{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Maybe where

data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just x) = f x
maybe y _ Nothing = y