import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbNumber 9000,
    DbString "Hello, world!",
    DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- filterDbDate :: [DatabaseItem] -> [UTCTime]
-- filterDbDate [] = []
-- filterDbDate ((DbDate time):xs) = [time] ++ filterDbDate xs
-- filterDbDate (_:xs) = filterDbDate xs

filterDbDateHelper :: DatabaseItem -> [UTCTime] -> [UTCTime]
filterDbDateHelper (DbDate time) list = list ++ [time]
filterDbDateHelper _ list = list

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate list = foldr filterDbDateHelper [] list

filterDbNumberHelper :: DatabaseItem -> [Integer] -> [Integer]
filterDbNumberHelper (DbNumber num) list = list ++ [num]
filterDbNumberHelper _ list = list

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber list = foldr filterDbNumberHelper [] list

mostRecentHelper :: DatabaseItem -> UTCTime -> UTCTime
mostRecentHelper (DbDate time) recentTime
  | time > recentTime = time
  | otherwise = recentTime
mostRecentHelper _ time = time

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent list = foldr mostRecentHelper (UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)) list

sumDb :: [DatabaseItem] -> Integer
sumDb list = foldr getSum 0 list
  where getSum a b =
          case a of DbNumber num -> num + b
                    _ -> b

avgDb :: [DatabaseItem] -> Double
avgDb list = (fromIntegral . fst $ sumCount) / (snd sumCount)
  where
    sumCount = foldr getAvg (0, 0) list
    getAvg a (sum, count) =
          case a of DbNumber num -> (num + sum, 1 + count)
                    _ -> (sum, count)
