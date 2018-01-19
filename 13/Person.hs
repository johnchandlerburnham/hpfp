--13/Person.hs
module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | Unknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ Unknown $ "Name was: " ++ show name 
                                         ++ " Age was: "  ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Name: "
  name <- getLine 
  putStrLn "Age: "
  age <- getLine
  let person = mkPerson name (read age) in
    case person of
      (Left _)  -> putStrLn ("Invalid person: " ++ (show person))
      (Right _) -> putStrLn ("Valid person: " ++ (show person))

-- best way to handle parse error for read is to change mkPerson
