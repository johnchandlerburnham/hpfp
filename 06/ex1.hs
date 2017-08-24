module Ex1 where

data Person = Person Bool

instance Show Person where show (Person x) = show x

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
