module SignalingAdversity.StringProcessing where

import Data.Maybe (fromMaybe)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s)
  where
    startsVowel = isVowel . head
    -- no word starting with a vowel is "the", so we can call 'go xs' instead of 'go (s:xs)'
    go ("the" : s : xs) = (if startsVowel s then 1 else 0) + go xs
    go (_ : xs) = go xs
    go [] = 0

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel
