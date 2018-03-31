isPal :: (Eq a) => [a] -> Bool
isPal x = x == reverse x

abs' :: Integer -> Integer
abs' x =
  if x < 0
    then -1 * x
   else
    x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))

r :: [a] -> [a]
r a = a

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = (bToC (aToB a))

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a
