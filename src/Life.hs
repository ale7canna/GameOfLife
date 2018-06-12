module Life where

import Data.List

data Cell = Cell Int Int
  deriving (Show, Eq, Ord)

class CellCapable a where
  cell :: a -> Cell

class LifeCycle a where
  evolve :: a -> [a] -> a

data Life = Alive Cell | Dead Cell
  deriving (Eq, Ord)

instance CellCapable Life where
  cell (Alive c) = c
  cell (Dead c)  = c

instance Show Life where
  show x@(Alive _) = "1"
  show x@(Dead _)  = "0"

show' :: [Life] -> String
show' x = foldr (\r res -> showRow r ++ "\n" ++ res) "" (splitRows x)

showRow :: [Life] -> String
showRow r = foldr (\e l -> show e ++ l) "" rows
  where rows = sortLifeByColumn r

sortLifeByColumn :: [Life] -> [Life]
sortLifeByColumn r = sortOn (\item -> snd $ getCoord $ cell item) r

splitRows :: [Life] -> [[Life]]
splitRows x = map (\row -> sort (splitArray (\l -> row == (fst $ getCoord $ cell l)) (x))) (getRows x)

getRows :: [Life] -> [Int]
getRows x = sort $ remDup rows
  where rows = [fst $ getCoord $ cell r | r <- x]

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
  | x `elem` xs = remDup xs
  | otherwise   = x : remDup xs

getCoord :: Cell -> (Int, Int)
getCoord (Cell r c) = (r, c)

splitArray :: (Life -> Bool) -> [Life] -> [Life]
splitArray f x = filter f x

instance LifeCycle Life where
  evolve x@(Alive c) world
    | alone x world     = Dead (c)
    | overPop x world   = Dead (c)
    | otherwise         = x
  evolve x@(Dead c) world
    | canBorn x world   = Alive (c)
    | otherwise         = x

alone :: Life -> [Life] -> Bool
alone = applyCondition (< 2)

overPop :: Life -> [Life] -> Bool
overPop = applyCondition (> 3)

canBorn :: Life -> [Life] -> Bool
canBorn = applyCondition (== 3)

applyCondition :: (Int -> Bool) -> Life -> [Life] -> Bool
applyCondition cond l oth = (cond . length . filter isAlive) (neighbours l oth)

isAlive :: Life -> Bool
isAlive (Alive _) = True
isAlive _ = False

life :: Int -> Int -> Life
life r c = Alive (Cell r c)

dead :: Int -> Int -> Life
dead r c = Dead (Cell r c)

neighbours :: Life -> [Life] -> [Life]
neighbours _ [] = []
neighbours life others =
  filter (areNeighbours life) others
  where areNeighbours = \c1 c2 -> isNear (cell (c1)) (cell (c2))

isNear :: Cell -> Cell -> Bool
isNear (Cell r1 c1) (Cell r2 c2) = abs(r2 - r1) <= 1 && abs (c2 - c1) <= 1