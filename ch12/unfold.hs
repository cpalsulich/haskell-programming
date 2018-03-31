data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = [(f a)] ++ (myIterate f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = (fst output) ++ myUnfoldr f (snd output)
  where getVals :: Maybe (a, b) -> b -> ([a], b)
        getVals Nothing b = ([], b)
        getVals (Just (a, b)) _ = ([a], b)
        output = getVals (f b) b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = constructTree (f a)
  where constructTree Nothing = Leaf
        constructTree (Just (left, root, right))
          = Node (unfold f left) root (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (func n) 0 
 where func :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
       func max x
         | x == max = Nothing
         | otherwise = Just (x + 1, x, x + 1)
