module Input where

import Life

loadChar :: Char -> Int -> Int -> Life
loadChar '1' r c = Alive (Cell r c)
loadChar '0' r c = Dead (Cell r c)
loadChar _ _ _ = error "Invalid input"

loadString :: String -> Int -> Int -> [Life]
loadString [] _ _ = []
loadString (x:xs) r c = loadChar x r c : loadString xs r (c + 1)

loadText :: String -> Int -> Int -> [Life]
loadText [] _ _ = []
loadText t r c =
  loadString s r c ++ loadText remaining (r + 1) c
  where s = head $ splitText t
        remaining = replaceChar (unwords (tail (splitText t))) ' ' '\n'

splitText :: String -> [String]
splitText x = words (replaceChar x '\n' ' ')

replaceChar :: String -> Char -> Char -> String
replaceChar [] _ _ = []
replaceChar (x:xs) a b
  | x == a    = b : replaceChar xs a b
  | otherwise = x : replaceChar xs a b