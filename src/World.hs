module World where

import Life

evolveWorld :: LifeCycle a => [a] -> [a]
evolveWorld x = map (evolve x) x

instance LifeCycle Life where
  evolve world x@(Alive c)
    | alone x world     = Dead (c)
    | overPop x world   = Dead (c)
    | otherwise         = x
  evolve world x@(Dead c)
    | canBorn x world   = Alive (c)
    | otherwise         = x

alone :: Life -> [Life] -> Bool
alone = applyCondition (< 2)

overPop :: Life -> [Life] -> Bool
overPop = applyCondition (> 3)

canBorn :: Life -> [Life] -> Bool
canBorn = applyCondition (== 3)

applyCondition :: (Int -> Bool) -> Life -> [Life] -> Bool
applyCondition cond l oth = (cond . length . filter isAlive) (neighbours l others)
  where others = filter (\e -> e /= l) oth


