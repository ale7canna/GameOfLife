module Life where

data Cell = Cell Int Int
  deriving (Show, Eq)

class CellCapable a where
  cell :: a -> Cell

data Life = Alive Cell | Dead Cell
  deriving (Show, Eq)

instance CellCapable Life where
  cell (Alive c) = c
  cell (Dead c)  = c

createLife :: Cell -> Life
createLife c = Alive c

createDead :: Cell -> Life
createDead c = Dead c

neighbours :: Life -> [Life] -> [Life]
neighbours _ [] = []
neighbours life others =
  filter (areNeighbours life) others
  where areNeighbours = \c1 c2 -> isNear (cell (c1)) (cell (c2))

isNear :: Cell -> Cell -> Bool
isNear (Cell r1 c1) (Cell r2 c2) = abs(r2 - r1) <= 1 && abs (c2 - c1) <= 1