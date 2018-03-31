module Print3 where

print2 :: IO ()
print2 = do
  putStrLn greeting
  where greeting = "Yarrr"

main :: IO ()
main = do
  putStrLn greeting
  print2
  where greeting = "Yarrr"
