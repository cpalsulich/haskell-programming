{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE FlexibleInstances #-}

import Data.Char

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Price =
  Price Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Airline Integer
  deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 100

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

areCars :: [Vehicle] -> [Bool]
areCars list = map isCar list

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

newtype Test =
  Test (Int, String) deriving (Eq, Show)

instance TooMany Test where
  tooMany (Test (n, _)) = tooMany n

instance TooMany (Int, Int) where
  tooMany (a, b) = tooMany (a + b)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany (a + b)

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

programmerVals = [(Programmer { lang = x, os = y }) | x <- allLanguages, y <- allOperatingSystems]

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf val@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf val ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords string
  = map (\word -> (word, [toUpper (head word)] ++ (tail word))) (words string)

capitalizeWord :: String -> String
capitalizeWord word = [toUpper (head word)] ++ (tail word)
