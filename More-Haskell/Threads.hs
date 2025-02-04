{-
https://www.youtube.com/watch?v=x3GwVccWcqs

Compile with:
  ghc -threaded Threads.hs

Run with:
  ./Threads +RTS -N
-}

import Control.Concurrent
import Data.Foldable
import System.IO

threadHello :: Chan () -> IO ()
threadHello endFlags = do
  tid <- myThreadId
  putStrLn $ "Hello from thread " ++ show tid
  writeChan endFlags ()

threadCount :: Int
threadCount = 10

main :: IO ()
main = do
  -- Disable buffering so we can see dirty writes to stdout
  hSetBuffering stdout NoBuffering

  -- Spawn threads
  endFlags <- newChan
  for_ [1 .. threadCount] $ do
    const $ forkIO $ threadHello endFlags

  -- Wait for all threads to complete
  for_ [1 .. threadCount] $ do
    const $ readChan endFlags
