{-
https://www.youtube.com/watch?v=x3GwVccWcqs

Compile with:
  ghc -threaded Threads.hs

Run with:
  ./Threads +RTS -N
-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO

withLock :: QSem -> IO a -> IO a
withLock = liftA2 bracket_ waitQSem signalQSem

threadHello :: QSem -> QSemN -> IO ()
threadHello stdoutLock threadFinished = do
  tid <- myThreadId
  let message = "Hello from thread " ++ show tid
  withLock stdoutLock $ putStrLn message
  signalQSemN threadFinished 1

threadCount :: Int
threadCount = 10

main :: IO ()
main = do
  -- Disable buffering so we can see dirty writes to stdout
  hSetBuffering stdout NoBuffering

  -- Spawn threads
  threadFinished <- newQSemN 0
  stdoutLock <- newQSem 1
  replicateM_ threadCount $ do
    forkIO $ threadHello stdoutLock threadFinished

  -- Wait for all threads to complete
  waitQSemN threadFinished threadCount
