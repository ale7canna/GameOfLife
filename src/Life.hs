module Life where

import Data.List
import Cell

class LifeCycle a where
  evolve :: [a] -> a -> a

data Life = Alive Cell | Dead Cell
  deriving (Eq, Ord)

instance CellCapable Life where
  cell (Alive c) = c
  cell (Dead c)  = c

instance Show Life where
  show x@(Alive _) = "1"
  show x@(Dead _)  = "0"

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
