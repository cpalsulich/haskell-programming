data TisAnInteger =
  TisAnInteger Integer

instance Eq TisAnInteger where
  (==) (TisAnInteger a) (TisAnInteger a') =
    a == a'

data TwoIntegers =
  TwoIntegers Integer Integer

instance Eq TwoIntegers where
  (==) (TwoIntegers a b) (TwoIntegers a' b') =
    a == a' && b == b'

data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
 (==) (TisAnInt a) (TisAnInt b) = a == b
 (==) (TisAString a) (TisAString b) = a == b

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v v1) (Pair v' v1') =
    v == v' && v1 == v1'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =
    a == a' && b == b'

data Which a =
  ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show)
