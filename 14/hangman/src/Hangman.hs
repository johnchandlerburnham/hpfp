module Hangman where

import Data.List (intersperse, nub)
import Data.Maybe (isJust)
import Control.Monad (forever)
import System.Exit (exitSuccess)

data Puzzle = Puzzle { word :: String, guessed :: String } deriving Show

discovered :: Puzzle -> [Maybe Char]
discovered p = (\x -> if elem x (guessed p) then Just x else Nothing) <$> word p

prettyPrint :: Puzzle -> String
prettyPrint p = concat
  [(intersperse ' ' $ printChar <$> discovered p)
  , "\n"
  , "Guessed so far: "
  , (guessed p)
  ]
 where
  printChar Nothing = '_'
  printChar (Just c) = c

newPuzzle :: String -> Puzzle
newPuzzle str = Puzzle str []

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  if elem guess (guessed puzzle)
  then
    do putStrLn "Already guessed that char, pick another."
       return puzzle
  else
    do if elem guess (word puzzle)
       then putStrLn "Guess successful!"
       else putStrLn "Guess not successful, try again"
       return $ Puzzle (word puzzle) (guess : (guessed puzzle))

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  putStrLn ""
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ prettyPrint puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Guess must be a single character"

gameOver :: Puzzle -> IO ()
gameOver puzzle =
  let g = guessed puzzle
      d = discovered puzzle
      w = word puzzle
      hangedNumber = (length g) - (length $ nub d)
  in
  if hangedNumber > 7
  then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ w
       exitSuccess
  else
    do putStr "Number of guesses: "
       print $ length g
       putStrLn $ hangedMan hangedNumber
       putStr "Number correct: "
       print $ length $ nub $ d
       return ()

gameWin :: Puzzle -> IO ()
gameWin puzzle =
  if all isJust (discovered puzzle)
  then
    do putStrLn "You win!"
       exitSuccess
  else return ()

hangedMan :: Int -> String
hangedMan 1 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |      O  \n"
  , "  |         \n"
  , "  |         \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan 2 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |      O  \n"
  , "  |      X  \n"
  , "  |         \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan 3 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |     \\O  \n"
  , "  |      X  \n"
  , "  |         \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan 4 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |     \\O/ \n"
  , "  |      X  \n"
  , "  |         \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan 5 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |     \\O/ \n"
  , "  |      X  \n"
  , "  |     /   \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan 6 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |     \\O/ \n"
  , "  |      X  \n"
  , "  |     / \\ \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan 7 = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |     \\Q/ \n"
  , "  |      X  \n"
  , "  |     / \\ \n"
  , "  |         \n"
  , "==+======== \n"
  ]
hangedMan _ = concat
  [ "  ________  \n"
  , "  |      |  \n"
  , "  |         \n"
  , "  |         \n"
  , "  |         \n"
  , "  |         \n"
  , "==+======== \n"
  ]
