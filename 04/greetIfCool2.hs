module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
  if cool coolness
    then putStrLn "eyyy"
  else
    putStrLn "pshh"
  where cool x = x == "frosty"
