module Main where

import Criterion.Main
import qualified Data.Sequence as S
import Data.Maybe (fromJust)

data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

qfromList :: [a] -> Queue a
qfromList l = Queue l []

class Pushable q where
  push :: a -> q a -> q a
  pop :: q a -> Maybe (a, q a)

instance Pushable Queue where
  push x (Queue es ds) = Queue (x:es) ds
  pop (Queue [] []) = Nothing
  pop (Queue es []) = pop $ Queue [] (reverse es)
  pop (Queue es (d:ds)) = Just (d, Queue es ds)

instance Pushable [] where
  push = (:)
  pop [] = Nothing
  pop xs = Just $ (last xs, init xs)

instance Pushable S.Seq where
  push x q = (S.|>) q x
  pop S.Empty = Nothing
  pop (a S.:<| s) = Just (a, s)

pushPop :: Pushable q => a -> q a -> Maybe (q a)
pushPop x q = do
  q' <- pop $ push x q
  return (snd q')

applyN :: Monad m => Int -> (a -> m a) -> m a -> m a
applyN 0 f ma = ma
applyN n f ma = (applyN (n - 1) f ma) >>= f

testList :: [Integer]
testList = [1..10000]

pushPopTest a n q = fromJust $ applyN n (pushPop a) (return q)

main :: IO ()
main = defaultMain
  [ bench "pushPop list" $ whnf (pushPopTest 0 500000) testList
  , bench "pushPop queue" $ whnf (pushPopTest 0 500000) (qfromList testList)
  , bench "pushPop seq" $ whnf (pushPopTest 0 500000) (S.fromList testList)
  ]
