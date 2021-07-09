module SignalingAdversity.MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee x _ Nothing = x

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes = map (fromMaybe undefined) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ((Just x):xs) = fmap (x:) (flipMaybe xs)
flipMaybe (Nothing:xs) = Nothing
flipMaybe [] = Just []
