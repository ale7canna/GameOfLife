module GameOfLife
    ( start
    ) where

import Input
import World
import Output
import Life

start :: IO ()
start = do
  input <- loadFile "world.gol"
  let world = loadText input 0 0
  putStrLn $ show'' $ run world 10

run :: [Life] -> Int -> [[Life]]
run x k = foldl (\res _ -> res ++ [evolveWorld (last res)]) [x] [1..k]

