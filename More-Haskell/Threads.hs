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

threadHello :: QSem -> Chan () -> IO ()
threadHello stdoutLock threadFinished = do
  tid <- myThreadId

  waitQSem stdoutLock
  putStrLn $ "Hello from thread " ++ show tid
  signalQSem stdoutLock

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
