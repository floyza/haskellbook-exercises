module AlgebraicDatatypes.Ciphers where

import Data.Char

repeatList :: [a] -> [a]
repeatList = foldr (++) [] . repeat

alphaToInt c
  | isUpper c = alphaToInt $ toLower c
  | otherwise = ord c - ord 'a'

intToAlpha i = chr (i + ord 'a')

caesar :: Int -> Char -> Char
caesar n c
  | not (isAlpha c) = c
  | isUpper c = toUpper $ caesar n (toLower c)
  | otherwise = chr (((ord c - ord 'a' + n) `mod` 26) + ord 'a')

uncaesar :: Int -> Char -> Char
uncaesar n = caesar (26 - n)

viginere :: String -> String -> String
viginere key = zipWith caesar (map alphaToInt $ repeatList key)

unviginere :: String -> String -> String
unviginere key = zipWith uncaesar (map alphaToInt $ repeatList key)
