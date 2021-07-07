module AlgebraicDatatypes.Phone where

import Data.List (elemIndex, foldl', group, sort, sortBy, maximumBy)
import Data.Char (toLower, isUpper)
import Data.Ord (comparing)

type Digit = Char
type Presses = Int

data Phone = Phone Digit [Char] Phone
           | Nil deriving Show

phone :: Phone
phone =
  Phone '1' "" $
  Phone '2' "abc" $
  Phone '3' "def" $
  Phone '4' "ghi" $
  Phone '5' "jkl" $
  Phone '6' "mno" $
  Phone '7' "pqrs" $
  Phone '8' "tuv" $
  Phone '9' "wxyz" $
  Phone '*' "^" $
  Phone '0' " +_" $
  Phone '#' ".," Nil

convo :: [String]
convo =
  ["This is a conversation",
   "Yeah it is"]

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps p@(Phone d s rest) c
  | isUpper c = reverseTaps p '^' ++ reverseTaps p (toLower c)
  | otherwise = case elemIndex c s of
      Just i -> [(d, i+1)];
      Nothing -> reverseTaps rest c
reverseTaps Nil _ = []

reverseTapsMsg :: Phone -> String -> [(Digit, Presses)]
reverseTapsMsg p = foldr (++) [] . map (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mergeTaps :: [(Digit, Presses)] -> (Digit, Presses) -> [(Digit, Presses)]
mergeTaps [] n = [n]
mergeTaps (old@(oldD,oldI):olds) new@(newD, newI)
  | oldD == newD = (oldD, newI+oldI) : olds
  | otherwise = old : mergeTaps olds new

-- Oops, wrote this instead of mostPopularLetter
mostPopularButton :: Phone -> String -> Char
mostPopularButton p = fst . maximumBy (comparing snd) . foldl' mergeTaps [] . reverseTapsMsg p

mostPopularLetter :: String -> Char
mostPopularLetter = head . head . sortBy (comparing (negate . length)) . group . sort

mostPopularWord :: String -> String
mostPopularWord = head . head . sortBy (comparing (negate . length)) . group . sort . words
