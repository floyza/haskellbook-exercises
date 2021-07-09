module SignalingAdversity.EitherLib where

lefts' :: [Either a b] -> [a]
--lefts' ((Left x):xs) = x:(lefts' xs)
--lefts' (_:xs) = lefts' xs
--lefts' [] = []

--lefts' = foldr (\ x acc ->
--                  case x of
--                  Left x -> x:acc
--                  Right _ -> acc)
--               []

lefts' = foldr (flip (\acc -> (either (:acc) (const acc)))) []

--rights' :: [Either a b] -> [b]
