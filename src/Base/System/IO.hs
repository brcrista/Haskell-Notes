{-# LANGUAGE NoImplicitPrelude #-}

module Base.System.IO where

import Base.Core.Classes ((==))
import Base.Data.Function ((.))
import Base.Data.List ((++))
import Base.Prelude.Base (String, return)
import GHC.IO
import GHC.Show (Show(..))
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