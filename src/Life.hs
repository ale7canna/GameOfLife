module Life where

data Cell = Cell Int Int
  deriving (Show, Eq)

data Life = Alive Cell | Dead Cell
  deriving (Show, Eq)

createLife :: Cell -> Life
createLife c = Alive c

createDead :: Cell -> Life
createDead c = Dead c

neighbours :: Life -> [Life] -> [Life]
neighbours _ [] = []
neighbours life others =
  filter (neigh life) others

neigh :: Life -> Life -> Bool
neigh (Alive c1) (Alive c2) = isNear c1 c2
neigh (Alive c1) (Dead c2)  = isNear c1 c2
neigh (Dead c1)  (Alive c2) = isNear c1 c2
neigh (Dead c1)  (Dead c2)  = isNear c1 c2

isNear :: Cell -> Cell -> Bool
isNear (Cell r1 c1) (Cell r2 c2) = abs(r2 - r1) <= 1 && abs (c2 - c1) <= 1