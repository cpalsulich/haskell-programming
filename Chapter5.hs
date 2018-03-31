-- Fix it
-- 1
--module Sing where
--
--fstString :: [Char] -> [Char]
--fstString x = x ++ " in the rain"
--
--sndString :: [Char] -> [Char]
--sndString x = x ++ " over the rainbow"
--
--sing = if (x < y) then fstString x else sndString y
--  where x = "Singing"
--        y = "Somewhere"
--
---- Earlier Exercises
--r :: [a] -> [a]
--r a = a
--
--co :: (b -> c) -> (a -> b) -> a -> c
--co bToC aToB a = (bToC (aToB a))
--
--a :: (a -> c) -> a -> a
--a _ a = a
--
--a' :: (a -> b) -> a -> b
--a' aToB a = aToB a



module Arith3Broken where

main :: IO ()
main = do
  print (1 + 2)
  print 10
  print (negate 1)
  print ((+) 0 blah)
    where blah = negate 1
  
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h a = g (f a)

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ x = fst (yToWZ (xToY x))
