module Input where

import System.IO
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
  fst $ foldl (\(res, r) x-> (res ++ loadString x r 0, r + 1) ) ([], 0) (splitLines t)

splitLines :: String -> [String]
splitLines x = case dropWhile (== '\n') x of
                    "" -> []
                    x' -> l : splitLines l'
                          where (l, l') = break (== '\n') x'

loadFile :: FilePath -> IO String
loadFile fp = do
  content <- readFile fp
  return content