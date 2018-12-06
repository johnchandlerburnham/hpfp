module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  threadDelay 1500
  hPutStr h (replicate 1000000000000000000 '0' ++ "abc")
  hClose h

data PleaseDie = PleaseDie deriving Show

instance Exception PleaseDie

main :: IO ()
main = do
  threadId <- forkIO openAndWrite
  threadDelay 1000
  throwTo threadId PleaseDie



