import Data.Bool
import Data.Char

eft :: (Enum a) => a -> a -> [a]
eft x y
  | (fromEnum y) <= (fromEnum x) = [y]
  | otherwise = [x] ++ (enumFromTo (succ x) y)

splitOn :: String -> Char -> [String]
splitOn input split
  | length input == 0 = []
  | otherwise =
      [takeWhile (/= split) input]
      ++ splitOn (dropWhile (==split) (dropWhile (/=split) input)) split

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

sqrCube = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 9.9 ex 6
mapAlt = [bool x (-x) (x==3) | x <- [1..10]]

multOf3 = length (filter (\x -> (rem x 3) == 0) [1..30])

removeArticles :: String -> [String]
removeArticles input = filter (\word -> not (elem word ["the", "a", "an"])) (words input)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = [(a,b)] ++ myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = [f a b] ++ myZipWith f as bs

myZipAlt = myZipWith (\x -> \y -> (x,y))

filterCaps :: String -> String
filterCaps string = filter isUpper string

capitalize :: String -> String
capitalize "" = ""
capitalize (firstLetter:rest) = [toUpper firstLetter] ++ rest

toCaps :: String -> String
toCaps "" = ""
toCaps (first:rest) = [toUpper first] ++ toCaps rest

capFirst :: String -> Char
capFirst = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x == True = True
  | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem val (x:xs)
  | val == x = True
  | otherwise = myElem val xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain list = squishMap (\x -> x) list

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f list
  | length list == 1 = first
  | f first altMax == GT = first
  | otherwise = altMax
  where
    first = head list
    altMax = myMaximumBy f (tail list)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f list
  | length list == 1 = first
  | f first altMin == LT = first
  | otherwise = altMin
  where
    first = head list
    altMin = myMinimumBy f (tail list)

myMaximum :: (Ord a) => [a] -> a
myMaximum list = myMaximumBy compare list

myMinimum :: (Ord a) => [a] -> a
myMinimum list = myMinimumBy compare list
