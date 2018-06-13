module Cell where

data Cell = Cell Int Int
  deriving (Show, Eq, Ord)

class CellCapable a where
  cell :: a -> Cell

isNear :: Cell -> Cell -> Bool
isNear (Cell r1 c1) (Cell r2 c2) = abs(r2 - r1) <= 1 && abs (c2 - c1) <= 1

getCoord :: Cell -> (Int, Int)
getCoord (Cell r c) = (r, c)