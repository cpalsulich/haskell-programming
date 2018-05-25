import Control.Monad
import Data.Semigroup
import Data.Monoid hiding((<>))
import Test.QuickCheck
import Debug.Trace

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
  
type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- 2
newtype Identity a = Identity a deriving Show
instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Eq a => Eq (Identity a) where
  Identity a == Identity b = a == b

instance (Arbitrary a, Monoid a, Semigroup a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc a = 
  a -> a -> a -> Bool

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- 3

data Two a b = Two a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Eq a, Eq b) => Eq (Two a b) where
  Two a b == Two a' b' = (a == a') && (b == b')

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

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

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)
  
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

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)
  
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

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (Show a, Show b) => Show (Combine a b) where
  show Combine {unCombine = f} = "Combine"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    return $ Combine a

combineAssoc :: (Eq b, Semigroup a, Semigroup b)
  => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc a b c d = unCombine (a <> (b <> c)) d == unCombine ((a <> b) <> c) d

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

combineRightIdentity :: Combine String String -> String -> Bool
combineRightIdentity combine string
  = (unCombine (mempty <> combine) string)
    == (unCombine combine string)

combineLeftIdentity :: Combine String String -> String -> Bool
combineLeftIdentity combine string
  = (unCombine (combine <> mempty) string)
    == (unCombine combine string)

-- Monoid 7

newtype Comp a = Comp (a -> a)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend (Comp f) (Comp f') = Comp (mappend f f')

instance Show (Comp a) where
  show _ = "Comp"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    a <- arbitrary
    return $ Comp a

compRightIdentity :: Comp String -> String -> Bool
compRightIdentity comp@(Comp f) string
  = (mappendF (mappend mempty comp) string) == (f string)
  where mappendF (Comp f') = f'

compLeftIdentity :: Comp String -> String -> Bool
compLeftIdentity comp@(Comp f) string
  = (mappendF (mappend comp mempty) string) == (f string)
  where mappendF (Comp f') = f'

-- Monoid 8

newtype Mem s a =
  Mem {
  runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem { runMem = \s -> (mempty, s) }
  mappend (Mem f) (Mem g) = Mem $ \s -> let (a1, s1) = f s
                                            (a2, s2) = g s1
                                        in (mappend a1 a2, s2)


instance Show (Mem s a) where
  show _ = "Mem"

instance (CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Mem a b) where
  arbitrary = do
    a <- arbitrary
    return $ Mem a

memRightIdentity :: Mem String String -> String -> Bool
memRightIdentity mem string
  = (runMem (mappend mempty mem) string)
    == (runMem mem string)

memLeftIdentity :: Mem String String -> String -> Bool
memLeftIdentity mem string
  = (runMem (mappend mem mempty) string)
    == (runMem mem string)


-- ****************************************************************

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool) 
  putStrLn "Identity"
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)  
  putStrLn "Two"
  quickCheck (semigroupAssoc :: (TwoAssoc String String))
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)  
  putStrLn "Three"
  quickCheck (semigroupAssoc :: (ThreeAssoc String String String))
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)  
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)  
  putStrLn "Or"
  quickCheck (semigroupAssoc :: (OrAssoc String String))
  putStrLn "Combine"
  quickCheck (combineAssoc :: (CombineAssoc String String))
  quickCheck combineRightIdentity
  quickCheck combineLeftIdentity
  putStrLn "Comp"
  quickCheck compRightIdentity
  quickCheck compLeftIdentity
  putStrLn "Mem"
  quickCheck memRightIdentity
  quickCheck memLeftIdentity
