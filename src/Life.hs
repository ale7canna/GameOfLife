module Life where

data Cell = Cell Int Int
  deriving (Show, Eq)

class CellCapable a where
  cell :: a -> Cell

class LifeCycle a where
  evolve :: a -> [a] -> a

data Life = Alive Cell | Dead Cell
  deriving (Show, Eq)

instance CellCapable Life where
  cell (Alive c) = c
  cell (Dead c)  = c

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