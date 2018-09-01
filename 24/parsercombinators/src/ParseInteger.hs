module ParseInteger where

import Text.Trifecta
import Text.Parser.Char
import Data.Char
import Text.Parser.Combinators
import Control.Monad
import Control.Applicative

parseDigit :: Parser Char
parseDigit = satisfy isDigit

base10Integer :: Parser Integer
base10Integer = go 0 where
  go a = do
    d <- parseDigit
    let a' = (10 * a + (digitToInt d))
    try (go a') <|> return (toInteger a')

--3
base10Integer' :: Parser Integer
base10Integer' = try (char '-' >> go 0) <|> base10Integer where
  go a = do
    d <- parseDigit
    let a' = (10 * a + (digitToInt d))
    try (go a') <|> return (negate $ toInteger a')

