module OurExceptions where

import Control.Exception

data EATD = NotEven Int | NotDivThree Int deriving (Eq, Show)

instance Exception EATD

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | even i = throwIO (NotEven i)
  | otherwise = return i


