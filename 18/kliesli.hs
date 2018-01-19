--18/kleisli.hs
import Control.Monad

sayHi :: String -> IO String
sayHi g = do
  putStrLn g
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
