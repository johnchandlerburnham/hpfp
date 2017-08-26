module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, nub)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = do 
  word <- randomWord'
  runGame $ freshPuzzle $ fmap toLower word

type WordList = [String]
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w = 
          let l = length (w :: String)
          in  minWordLength < l && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do 
  randomIndex <- randomRIO (0, (length wl) - 1) 
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
instance Show Puzzle where 
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _ ) char = elem char str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed  (Puzzle _ _ guessed) char = elem char guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledIn s) c = Puzzle word newFilledIn (c : s) 
  where newFilledIn = zipWith (zipp c) word filledIn
        zipp guess wordChar fillChar = if   guess == wordChar
                                       then Just wordChar
                                       else fillChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
    (_, True) -> do 
      putStrLn "Already guessed that char, pick another."
      return puzzle
    (True, _) -> do 
      putStrLn "Guess successful!"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "Guess not successful, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledIn guessed) =
  if ((length guessed) - (length $ nub filledIn)) > 7
  then 
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else 
    do print (length guessed)
       print (filledIn)
       print (nub filledIn)
       print (length $ nub filledIn)
       return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledIn _) = 
  if all isJust filledIn 
  then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Guess must be a single character"

    
