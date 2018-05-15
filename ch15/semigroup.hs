import Control.Monad
import Data.Semigroup
import Test.QuickCheck

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
  
type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a
instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Eq a => Eq (Identity a) where
  Identity a == Identity b = a == b

instance Arbitrary (Identity a) where
  arbitrary = do
    Identity a <- arbitrary
    return (Identity a)

type IdentityAssoc a = 
  a -> a -> a -> Bool

-- 3

data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Eq a, Eq b) => Eq (Two a b) where
  Two a b == Two a' b' = (a == a') && (b == b')

instance (Show a, Show b) => Show (Two a b) where
  show (Two a b) = "Two: " ++ (show a) ++ " " ++ (show b)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = 
  Two a b -> Two a b -> Two a b -> Bool

-- 4

data Three a b c = Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where
  Three a b c == Three a' b' c' = (a == a') && (b == b') && (c == c')

instance (Show a, Show b, Show c) => Show (Three a b c) where
  show (Three a b c) = "Three: " ++ (show a)
                     ++ " " ++ (show b)
                     ++ " " ++ (show c)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc a b c= 
  Three a b c -> Three a b c -> Three a b c -> Bool

-- 6

newtype BoolConj = BoolConj Bool

instance Semigroup BoolConj where
  BoolConj a <> BoolConj b = BoolConj (a && b)

instance Eq BoolConj where
  BoolConj a == BoolConj b = a == b

instance Show BoolConj where
  show (BoolConj a) = "BoolConj: " ++ (show a)
  
instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc = 
  BoolConj -> BoolConj -> BoolConj -> Bool

-- 7

newtype BoolDisj = BoolDisj Bool

instance Semigroup BoolDisj where
  BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Eq BoolDisj where
  BoolDisj a == BoolDisj b = a == b

instance Show BoolDisj where
  show (BoolDisj a) = "BoolDisj: " ++ (show a)
  
instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = 
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8

data Or a b = Fst a | Snd b

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Snd x <> _ = Snd x
  _ <> x = x

instance (Eq a, Eq b) => Eq (Or a b) where
  Fst _ == Snd _ = False
  Snd _ == Fst _ = False
  Fst a == Fst a' = a == a'
  Snd a == Snd a' = a == a'

instance (Show a, Show b) => Show (Or a b) where
  show (Fst a) = "Or (Fst): " ++ (show a)
  show (Snd a) = "Or (Snd): " ++ (show a)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    frequency [(1, liftM Fst arbitrary),
               (1, liftM Snd arbitrary)]

type OrAssoc a b = 
  Or a b -> Or a b -> Or a b -> Bool

-- 9

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  Combine {unCombine = f} <> Combine {unCombine = f'}
    = Combine {unCombine = combine f f'}
    where combine f1 f2 bind = f1 bind <> f2 bind

instance (Show a, Show b) => Show (Combine a b) where
  show Combine {unCombine = f} = "Combine"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    return $ Combine a

combineAssoc :: (Eq b, Semigroup a, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc a b c d = unCombine (a <> (b <> c)) d == unCombine ((a <> b) <> c) d

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

-- ****************************************************************

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn "Identity"
  quickCheck (semigroupAssoc :: (IdentityAssoc String))
  putStrLn "Two"
  quickCheck (semigroupAssoc :: (TwoAssoc String String))
  putStrLn "Three"
  quickCheck (semigroupAssoc :: (ThreeAssoc String String String))
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn "Or"
  quickCheck (semigroupAssoc :: (OrAssoc String String))
  putStrLn "Combine"
  quickCheck (combineAssoc :: (CombineAssoc String String))
