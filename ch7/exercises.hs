addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- functionC x y = if (x > y) then x else y
functionC x y =
  case (x > y) of
    True -> x
    False -> y

-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n =
  case even n of
    True -> (n+2)
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    _ -> 0

tensDigit :: Integral a => a -> a
tensDigit = ((flip mod) 10) . ((flip div) 10)
--tensDigit x = (mod (div x 10) 10)
-- tensDigit x = d
--   where xLast = x `div` 10
--         d = xLast `mod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where xLast = x `div` 100
        d2 = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b == True = x
  | otherwise = y
-- foldBool x y b =
--   case b of
--     True -> x
--     False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)
