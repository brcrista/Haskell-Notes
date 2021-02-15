{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.IO where

import GHC.IO
import GHC.Show (Show)
import Prelude.Base (String)

-- getLineRecursive :: String -> IO String
-- getLineRecursive xs = do
--   nextChar <- getChar
--   if nextChar == '\n'
--     then return xs
--     else getLineRecursive (xs ++ [nextChar])

-- getLine :: IO String
-- getLine = getLineRecursive []

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x : xs) = do
--   putChar x
--   putStr xs

-- putStrLn :: String -> IO ()
-- putStrLn xs = do
--   putStr xs
--   putChar '\n'

-- print :: Show a => a -> IO ()
-- print = putStrLn . show