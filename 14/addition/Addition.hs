-- /14/addition/Addition.hs
module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      1 + 1  > (1 :: Integer) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2  `shouldBe` (4 :: Integer)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` ((5, 0) :: (Integer, Integer))
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` ((4, 2) :: (Integer, Integer))
  describe "Multiplication" $ do
    it "1 * 1 is 1" $ do
      rMult 1 1 `shouldBe` (1 :: Integer)
    it "2 * 2 is 4" $ do
      rMult 2 2 `shouldBe` (4 :: Integer)
    it "2 * 0 is 0" $ do
      rMult 2 0 `shouldBe` (0 :: Integer)
    it "0 * 2 is 0" $ do
      rMult 0 2 `shouldBe` (0 :: Integer)
    it "0 * 0 is 0" $ do
      rMult 0 0 `shouldBe` (0 :: Integer)
    it "3 * 4 is 12" $ do
      rMult 3 4 `shouldBe` (12 :: Integer)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

rMult :: (Integral a) => a -> a -> a
rMult x y = go x y 0
  where go a b c
          | b == 0 = c
          | otherwise = go a (b - 1) (c + a)

sayHello :: IO ()
sayHello = putStrLn "hello!"
