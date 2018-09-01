module RandomExample where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix
           deriving (Eq, Show)

{-
intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
-- Use 'error'
-- _extremely_ sparingly.
  x -> error $ "intToDie got non 1-6 integer: " ++ show x
-}
-- partial functions are baaaad

intToDie :: Int -> Die
intToDie n =
  case ((n `mod` 6)) of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    0 -> DieSix

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

-- 1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g= go 0 0 n g
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go sum count max gen
      | sum >= max = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) max nextGen

-- 2
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] n g
  where
    go :: Int -> Int -> [Die] -> Int -> StdGen -> (Int, [Die])
    go sum count log max gen
      | sum >= max = (count, log)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) ((intToDie die):log) max nextGen




