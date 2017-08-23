module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
  if cool
    then putStrLn "eyyyyy."
  else
    putStrLn "pshhhh."
  where cool = coolness == "frosty"
