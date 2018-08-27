module Main where

import Cipher
import Test.QuickCheck
import Data.Char (isAsciiLower)

lowerGen :: Gen Char
lowerGen = suchThat arbitrary (isAsciiLower)

newtype LowerString = LowerString String deriving (Show, Eq)

instance Arbitrary LowerString where
  arbitrary = LowerString <$> (listOf1 lowerGen)

caesarProp :: Int -> LowerString -> Bool
caesarProp key (LowerString string) =
  string == (unCaesar key $ caesar key string)

vignereProp :: LowerString -> LowerString -> Bool
vignereProp (LowerString key) (LowerString string) =
  string == (unVignere key $ vignere key string)

main :: IO ()
main = do
  quickCheck caesarProp
  quickCheck vignereProp
