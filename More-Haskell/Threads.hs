{-
https://www.youtube.com/watch?v=x3GwVccWcqs

Compile with:
  ghc -threaded Threads.hs

Run with:
  ./Threads +RTS -N
-}

import Control.Concurrent
import Control.Monad
import System.IO

putStrLnLocked :: QSem -> String -> IO ()
putStrLnLocked lock message = do
  waitQSem lock
  putStrLn message
  signalQSem lock

threadHello :: QSem -> Chan () -> IO ()
threadHello stdoutLock threadFinished = do
  tid <- myThreadId
  let message = "Hello from thread " ++ show tid
  putStrLnLocked stdoutLock message
  writeChan threadFinished ()

threadCount :: Int
threadCount = 10

main :: IO ()
main = do
  -- Disable buffering so we can see dirty writes to stdout
  hSetBuffering stdout NoBuffering

  -- Spawn threads
  threadFinished <- newChan
  stdoutLock <- newQSem 1
  replicateM_ threadCount $ do
    forkIO $ threadHello stdoutLock threadFinished

  -- Wait for all threads to complete
  replicateM_ threadCount $ do
    readChan threadFinished
