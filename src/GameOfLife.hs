module GameOfLife
    ( start
    ) where

import Life
import Input

start :: IO ()
start = do
  input <- loadFile "world.gol"
  putStrLn (show' $ loadText input 0 0)

