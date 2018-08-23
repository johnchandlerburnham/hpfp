module ExitSuccess where

import Data.Char
import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if isPalindrome line1
  then putStrLn "It's a palindrome!"
  else do putStrLn "nope"
          exitSuccess

isPalindrome :: String -> Bool
isPalindrome str = (str' == reverse str')
  where str' = filter isAlpha $ map toLower str
