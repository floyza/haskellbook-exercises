module AlgebraicDatatypes.AsPatterns where

import Data.Char

isSubseqOf :: (Eq a)
              => [a]
              -> [a]
              -> Bool
isSubseqOf xl@(x:xs) (s:ss)
  | x == s = isSubseqOf xs ss
  | otherwise = isSubseqOf xl ss
isSubseqOf [] _ = True
isSubseqOf _ [] = False

capitalizeWords :: String
                -> [(String, String)]
capitalizeWords = map (\ s@(x:xs) -> (s, toUpper x : xs)) . words
