module Main where

import Data.Char

import System.IO
import Data.IORef
import System.Environment


data EncryptMode = Decrypt | Encrypt | Unset

main :: IO ()
main = do
  args <- getArgs
  mode <- newIORef Unset
  key <- newIORef ""
  case args of
    ["-d", k] -> writeIORef mode Decrypt >> writeIORef key k
    ["-e", k] -> writeIORef mode Encrypt >> writeIORef key k
    _      -> printHelp >> return ()
  key <- readIORef key
  mode <- readIORef mode
  b <- hWaitForInput stdin 2000
  if b
  then do
    input <- hGetContents stdin
    let out = case mode of
                Decrypt -> unVignere key input
                Encrypt -> vignere key input
    hPrint stdout out
  else do
    hPrint stderr "timed-out"
  return ()

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: "
  putStrLn "\t-d : decrypt stdin, print to stdout"
  putStrLn "\t-e : encrypt stdin, print to stdout"

vignere :: String -> String -> String
vignere key cleartext = map caeserHelper $ zip key' clr' where
    pre = (map toLower . filter isAlpha)
    clr' = (pre cleartext)
    key' = take (length clr') $ cycle (pre key)
    caeserHelper (a, b) = chr ((ord a + ord b - 2*ord 'a') `mod` 26 + ord 'a')

unVignere :: String -> String -> String
unVignere key ciphertext = map caeserHelper $ zip key' ciphertext where
    pre = (map toLower . filter isAlpha)
    key' = take (length ciphertext) $ cycle (pre key)
    caeserHelper (a, b) = chr ((ord b - ord a) `mod` 26 + ord 'a')

