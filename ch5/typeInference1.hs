f :: Num a => a -> a -> a
f x y = x + y + 3

myConcat x = x ++ " yo"


f1 :: Int -> String
f1 a = show a

g :: String -> Char
g a = head a

h :: Int -> Char
h a = head (show a)
