{-# LANGUAGE NoImplicitPrelude #-}

module Base.Prelude.IO where

import GHC.IO
import GHC.Show (Show(..))
import Base.Prelude.Base (String, (++), (.), return)
import Base.Prelude.Classes ((==))
import System.IO(getChar, putChar)

getLineRecursive :: String -> IO String
getLineRecursive xs = do
  nextChar <- getChar
  if nextChar == '\n'
    then return xs
    else getLineRecursive (xs ++ [nextChar])

getLine :: IO String
getLine = getLineRecursive []

putStr :: String -> IO ()
putStr [] = return ()
putStr (x : xs) = do
  putChar x
  putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do
  putStr xs
  putChar '\n'

print :: Show a => a -> IO ()
print = putStrLn . show