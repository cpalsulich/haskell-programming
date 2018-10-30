{-# LANGUAGE ViewPatterns #-}
import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1
newtype Identity a = Identity a
  deriving (Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Eq a => Eq (Identity a) where
  Identity a == Identity b = a == b

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (CoArbitrary a, Arbitrary a) => CoArbitrary (Identity a) where
  coarbitrary = do
    a <- arbitrary
    return (Identity a)

type IdToId = Fun (Identity String) (Identity String)

type IdFC = [Identity String] -> IdToId -> IdToId -> Bool

main :: IO ()
main = do
  quickCheck (functorCompose' :: IdFC)
