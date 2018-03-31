isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just val) = val : []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : (catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe input = foldl foldFunc (Just []) input
  where
    foldFunc :: Maybe [a] -> Maybe a -> Maybe [a]
    foldFunc Nothing _ = Nothing
    foldFunc _ Nothing = Nothing
    foldFunc (Just list) (Just a) = Just (a : list)
