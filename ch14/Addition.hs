module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((2 :: Integer) > 1) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (4 :: Integer) `shouldBe` (4 :: Integer)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)

multiply :: (Eq a, Num a) => a -> a -> a
multiply _ 0 = 0
multiply num val = num + (multiply num (val - 1))
