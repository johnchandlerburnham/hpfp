{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module WhySomeException where

import Control.Exception
  ( ArithException(..)
  , AsyncException(..))

import Data.Typeable

data MyException = forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError 0 = Left (MyException DivideByZero)
multiError 1 = Left (MyException StackOverflow)
multiError n = Right n

data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving Show

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing -> case cast e of
      (Just async) -> Async async
      Nothing -> SomethingElse

runDisc n = either discriminateError (const SomethingElse) (multiError n)
