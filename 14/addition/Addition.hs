-- /14/addition/Addition.hs
module Addition where 

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "Multiplication" $ do
    it "1 * 1 is 1" $ do
      rMult 1 1 `shouldBe` 1
    it "2 * 2 is 4" $ do
      rMult 2 2 `shouldBe` 4
    it "2 * 0 is 0" $ do
      rMult 2 0 `shouldBe` 0
    it "0 * 2 is 0" $ do
      rMult 0 2 `shouldBe` 0
    it "0 * 0 is 0" $ do
      rMult 0 0 `shouldBe` 0
    it "3 * 4 is 12" $ do
      rMult 3 4 `shouldBe` 12

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

rMult :: (Integral a) => a -> a -> a
rMult a b = go a b 0
  where go a b c
          | b == 0 = c
          | otherwise = go a (b - 1) (c + a)

sayHello :: IO ()
sayHello = putStrLn "hello!"
