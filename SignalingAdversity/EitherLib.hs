module SignalingAdversity.EitherLib where

import Control.Arrow

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left x) = (x :)
    f (Right _) = id

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right x) acc = x : acc
    f (Left _) acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = lefts' &&& rights'

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
