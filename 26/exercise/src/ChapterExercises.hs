module ChapterExercises where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity

-- 1
rDec :: Num a => Reader a a
rDec = ReaderT $ \x -> return (x - 1)

-- 2
rDecPF :: Num a => Reader a a
rDecPF = reader $ decrement where
  decrement = (flip (-)) 1

-- 3
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \x -> return (show x)

-- 4
rShowPF :: Show a => ReaderT a Identity String
rShowPF = reader show

-- 5
printGreeting :: Show a => a -> IO ()
printGreeting r = putStrLn $ "Hi: " ++ show r

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> (liftIO $ printGreeting r) >> return (r + 1)

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftIO $ printGreeting s
  return (show s, s + 1)

-- Fix the code
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
