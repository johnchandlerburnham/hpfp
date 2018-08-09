module DivModQuotRemCompare where

divModQuotRem :: Integer -> Integer -> IO () 
divModQuotRem x y = do
  let nx = negate x
  let ny = negate y
  let printExp f x y = putStr $ f ++ " " ++ (show x) ++ " " ++ show y  ++ " = "
  printExp "divMod" x y
  print $ divMod x y
  printExp "quotRem" x y
  print $ quotRem x y
  printExp "divMod" nx y
  print $ divMod nx y
  printExp "quotRem" nx y
  print $ quotRem nx y
  printExp "divMod" x ny
  print $ divMod x ny
  printExp "quotRem" x ny
  print $ quotRem x ny
  printExp "divMod" nx ny
  print $ divMod nx ny
  printExp "quotRem" nx ny
  print $ quotRem nx ny

sign x = div (abs x) x

quot' x y = (sign x) * (sign y) * (div (abs x) (abs y))
rem' x y = x

