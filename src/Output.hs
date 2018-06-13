module Output where

import Data.List
import Life
import Cell

show' :: [Life] -> String
show' x = foldr (\r res -> showRow r ++ "\n" ++ res) "" (splitRows x)

showRow :: [Life] -> String
showRow r = foldr (\e l -> show e ++ l) "" rows
  where rows = sortOn (\e -> getColumn e) r

splitRows :: [Life] -> [[Life]]
splitRows x = map (\row -> sort (filter (\l -> row == getRow l) x)) (getRows x)

getRows :: [Life] -> [Int]
getRows x = sort $ remDup rows
  where rows = [getRow r | r <- x]

getRow :: Life -> Int
getRow = fst . getCoord . cell

getColumn :: Life -> Int
getColumn = snd . getCoord . cell

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
  | x `elem` xs = remDup xs
  | otherwise   = x : remDup xs