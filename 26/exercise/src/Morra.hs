module Morra where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.Console.ANSI
import System.Random
import System.Environment (getArgs)

data Throw = One | Two deriving (Eq, Show)
data Parity = Odd | Even deriving (Eq, Show, Read)
type Round = (Throw, Throw)
type Score = (Int, Int)
type Victory = Bool

data Algorithm = RandomA | TrigramA deriving (Eq, Show)
data Config = Config {
    victory :: Int
  , p1WinsOn :: Parity
  , isP2AI :: Bool
  , algo :: Algorithm
  } deriving (Eq, Show)

data GameState = GameState { 
    score    :: Score
  , rounds   :: [Round]
  , config   :: Config
  } deriving (Eq, Show)

roundParity :: Round -> Parity
roundParity (x, y) = if x == y then Even else Odd

processRound :: Round -> GameState -> GameState
processRound round old = GameState newScore (round:(rounds old)) (config old)
  where (p1, p2) = score old
        newScore = if roundParity round == (p1WinsOn $ config $ old) 
                   then (p1 + 1, p2) 
                   else (p1, p2 + 1)

readThrow :: String -> Throw
readThrow "1" = One
readThrow _   = Two

randomThrow :: IO Throw
randomThrow = do 
  n <- randomRIO (1, 2) 
  if n == (1 :: Int) then return One 
  else return Two

trigrams :: [a] -> [(a,a,a)]
trigrams xs = go xs []
  where 
    go (x1:x2:x3:xs) ts = go (x2:x3:xs) $ (x1,x2,x3):ts
    go _     ts = ts

trigramThrow :: (Throw, Throw) -> [(Throw, Throw, Throw)] -> IO Throw
trigramThrow (t1, t2) ts = do
  let freq t3 = length $ filter (==(t1, t2, t3)) ts
      pick EQ = randomThrow
      pick GT = return One
      pick LT = return Two
  pick (compare (freq One) (freq Two))
   
aiThrow :: [Round] -> Algorithm -> IO Throw
aiThrow rounds RandomA = randomThrow
aiThrow rounds TrigramA =
  let throws = map fst rounds
  in trigramThrow (last $ init throws, last throws) (trigrams throws)

getRound :: GameState -> IO Round
getRound st = do 
  let isP2AI' = isP2AI $ config st 
  putStr "Player 1's Throw:"
  p1Throw <- readThrow <$> getLine
  if isP2AI' then do
    putStr "Player 2's Throw:"
    p2Throw <- aiThrow (rounds st) (algo $ config st)
    print p2Throw
    return (p1Throw, p2Throw)
  else do
    gameFrame st
    putStr "Player 2's Throw:"
    p2Throw <- readThrow <$> getLine
    return (p1Throw, p2Throw)

gameFrame :: GameState -> IO ()
gameFrame st = do
  let (p1, p2) = score st
  clearScreen
  setCursorPosition 0 0 
  liftIO $ putStrLn $ "Scores: P1 " ++ show p1 ++ ", P2 " ++ show p2

game :: StateT GameState IO Victory
game = do
  st <- get
  let score' = score st
  liftIO $ gameFrame st
  let done (a, b) = a >= 3 || b >= 3
  if done score' then return ((fst score') >= 3)
  else do
    newRound <- liftIO $ getRound st
    modify (processRound newRound) 
    game
 
parseOptions :: [String] -> Config
parseOptions [end, win, ai] = Config (read end) (read win) (read ai) TrigramA
parseOptions _ = Config 3 Odd True RandomA

main :: IO ()
main = do
  config <- parseOptions <$> getArgs
  let init = GameState (0,0) [] config
  print init
  putStrLn "Let's get ready for some Morra."
  (win, final) <- runStateT game init
  if win then putStrLn "You Win!"  
  else putStrLn "You Lose."
