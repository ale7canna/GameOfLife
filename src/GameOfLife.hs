module GameOfLife
    ( start
    ) where

import Input
import World
import Output

start :: IO ()
start = do
  input <- loadFile "world.gol"
  let world = loadText input 0 0
  putStrLn (show' $ world)
  putStrLn "\n"
  putStrLn $ show' (evolveWorld world)


