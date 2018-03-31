cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"

frappe = flippy "haha"

summation :: (Ord a, Num a) => a -> a
summation x
  | 0 < x = x + (summation (x-1))
  | otherwise = x

recMult :: (Integral a) => a -> a -> a
recMult x y
  | 0 < y = x + (recMult x (y - 1))
  | y < 0 = (recMult x (y + 1)) - x
  | otherwise = 0

mc91 ::  (Num a, Ord a) => a -> a
mc91 x
  | 100 < x = x - 10
  | otherwise = mc91 . mc91 $ (x + 11)
