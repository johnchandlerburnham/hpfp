module ParseTest where

import ParseLog
import Data.Maybe
import Data.Fixed
import Data.List (isInfixOf)
import Data.Char (isSpace, isPrint)
import Test.QuickCheck
import Data.Time


instance Arbitrary Day where
  arbitrary = fromJust <$> suchThat day' isJust
    where day' = fromGregorianValid <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TimeOfDay where
  arbitrary = do
    let hour = choose (0, 23)
    let minute = choose (0, 59)
    let wholeSecond = (read . show) <$> choose (0 :: Integer, 59 :: Integer)
    let second = frequency [(1, wholeSecond), (1, arbitrary)]
    let time' = makeTimeOfDayValid <$> hour <*> minute <*> second
    fromJust <$> suchThat time' isJust

instance Arbitrary Log where arbitrary = Log <$> listOf1 arbitrary

instance Arbitrary Entry where
  arbitrary = Entry <$> (Right <$> arbitrary) <*> (listOf1 arbitrary)

instance Arbitrary Line where
  arbitrary = Line <$> (Right <$> arbitrary) <*> arbitrary

instance Arbitrary Activity where 
  arbitrary = do
    let validChar = suchThat arbitrary (\x -> isPrint x && '\n' /= x)
    let validActivity x = (not $ isInfixOf "--" x) && 
                          (not $ isSpace $ head x) &&
                          (not $ isSpace $ last x)
    Activity <$> suchThat (listOf1 validChar) validActivity

main :: IO ()
main = do
  quickCheck logBimorphism
