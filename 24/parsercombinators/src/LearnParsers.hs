--24/parsercombinators/src/LearnParser.hs
module LearnParsers where

import Text.Trifecta
import Text.Parser.Char
import Text.Parser.Combinators
import Control.Monad

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParseStr :: Parser String -> String -> IO ()
testParseStr p str = print $ parseString p mempty str

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- 1
oneFail :: Parser Char
oneFail = one >> eof >> stop

oneTwoFail :: Parser Char
oneTwoFail = oneTwo >> eof >> stop

-- 2
oneTwoThree :: Parser String
oneTwoThree = some $ oneOf "123"

oneTwoThree' :: Parser String
oneTwoThree' =  choice [string "123", string "12", string "1"]

oneTwoThree'' :: Parser String
oneTwoThree'' = 
  char '1' >>= \x -> (char '2') >>= \x2 -> (char '3') >>= \x3 -> return [x,x2,x3]


oneTwoThreeDo :: Parser String
oneTwoThreeDo = do
  x1 <- (char '1')
  x2 <- (char '2')
  x3 <- (char '3')
  return [x1,x2,x3]

-- 3
string' :: String -> Parser String
string' "" = return ""
string' (x:xs) = do
  char x
  string' xs
  return (x:xs)

string'' :: String -> Parser String
string'' "" = return ""
string'' (x:xs) = do
  a <- char x
  b <- string'' xs
  return (a:b)

