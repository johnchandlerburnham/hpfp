import System.IO.Unsafe

unsafe :: Int
unsafe = unsafePerformIO (print "a hole in the simulation" >> return 2)

main :: IO ()
main = do
  putStrLn "the outside perspective"
  print $ 2 + unsafe

