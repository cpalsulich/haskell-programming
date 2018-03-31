-- foldl (flip (*)) 1 [1..3]
-- (((1 * 3) * 2) * 1)

fibs = 1 : scanl (+) 1 fibs
fibsN x = take x fibs
fibsX x = takeWhile x fibs

facto = scanl (*) 1 [1..]
factorial x = facto !! x
-- 1 * 2 * 3 * 4 * 5
-- [1, 2, 6, 24, 120]

stops = "pbtdkg"
vowels = "aeiou"

svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> (f a) || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem val list = any (\x -> x == val) list
-- myElem x = foldr (\a b -> (x == a) || b) False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myMap :: (a -> b) -> [a] -> [b]
myMap f list = foldr (\a b -> [f a] ++ b) [] list

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f list = foldr include [] list
  where include a b
          | f a = [a] ++ b
          | otherwise = b

squish :: [[a]] -> [a]
squish list = foldr (\a b -> a ++ b) [] list

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f list = foldr (\a b -> (f a) ++ b) [] list

squishAgain :: [[a]] -> [a]
squishAgain list = squishMap (\x -> x) list

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if ((f a b) == GT) then a else b) x xs
