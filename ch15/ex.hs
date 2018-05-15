import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (mappend a b)

----------------------------------------------------------------

monoidAssoc :: (Eq m, Monoid m)
  => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
  => m
  -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
  => m
  -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = firstMappend

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend (First' (Only a)) _ = First' (Only a)
firstMappend (First' Nada) (First' (Only a)) = First' (Only a)
firstMappend (First' Nada) (First' Nada) = First' Nada

instance Arbitrary (First' a) where
  arbitrary = do
    First' a <- arbitrary
    frequency [ (1, return (First' (Nada)))
              , (1, return (First' a)) ]

type FirstMappend =
    First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
