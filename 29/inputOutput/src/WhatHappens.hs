module WhatHappens where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  -- mv' <- myData
  zero <- takeMVar mv
  print zero

unsafeMyData :: MVar Int
unsafeMyData = unsafePerformIO newEmptyMVar

unsafeMain :: IO ()
unsafeMain = do
  putMVar usafeMyData 0
  zero <- takeMVar unsafeMyData
  print zero

