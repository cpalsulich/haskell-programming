-- data Person = Person Bool
-- 
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)

-- data Mood = Blah
--   | Woot deriving Show
-- 
-- settleDown x = if x == Woot
--   then Blah
--   else x

import Data.List (sort)


type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- i :: a
i = 1

f :: RealFrac a => a
f = 1.0

myX = 1
sigmund' :: Num a => a -> a
sigmund' x = x


young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

arith :: Num b => (a -> b)
      -> Integer -> a -> b

arith aToB int a = (fromInteger int) + (aToB a)
