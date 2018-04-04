--24/parsercombinators/src/TextFractions.hs
{-# LANGUAGE OverloadedStrings #-}
module TextFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

testVirtuous :: IO ()
testVirtuous = do
  let parseFraction' = parseString virtuousFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

-- Exercise: Unit of Success
unitSuccess :: String -> Parser String
unitSuccess str = do
  a <- string str
  eof
  return a

-- Try Try

parseDecimal :: Parser Rational
parseDecimal = do
  whole <- decimal
  char '.'
  part <- decimal
  case part of
    0 -> return (toRational whole)
    _ -> return (makeDecimal whole part)

makeDecimal :: Integer -> Integer -> Rational
makeDecimal whole part = (toRational whole) + (toRational frac) where
  sigDigits = (floor $ logBase 10 $ fromIntegral part) + 1
  frac = (fromIntegral part) / (10 ^ sigDigits) 

parseFracOrDec :: Parser Rational
parseFracOrDec = try virtuousFraction <|> try parseDecimal

