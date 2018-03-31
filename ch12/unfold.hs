myIterate :: (a -> a) -> a -> [a]
myIterate f a = [(f a)] ++ (myIterate f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = (fst output) ++ myUnfoldr f (snd output)
  where getVals :: Maybe (a, b) -> b -> ([a], b)
        getVals Nothing b = ([], b)
        getVals (Just (a, b)) _ = ([a], b)
        output = getVals (f b) b
