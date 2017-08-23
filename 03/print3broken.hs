module Print3Broken where

printSecond :: IO ()
printSecond = do
  putStrLn greeting
  where greeting = "Yarr"

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where greeting = "Yarr"
