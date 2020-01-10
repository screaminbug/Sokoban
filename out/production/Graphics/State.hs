module State where

import Coord
import Direction
import Maze
import Tiles
import Boxes
import ListUtil

type Level = Int

data WinState = Playing | LevelWon | GameWon deriving Eq
data State = S Direction Coord [Coord] Level WinState

initialDirection :: Direction
initialDirection = L

nthMaze :: Level -> Maze
nthMaze level
  | level < listLength mazes = mazes !! level
  | otherwise = head mazes
  
nthLevel :: Level -> Coord -> Tile
nthLevel level = getLevel $ nthMaze level
  where
    getLevel (Maze _ maze) = maze

nthInitialCoord :: Level -> Coord
nthInitialCoord level = getCoord $ nthMaze level
  where
    getCoord (Maze c _) = c

initialBoxPositions :: Level -> [Coord]
initialBoxPositions level = foldAppendList $ boxesInRow $ nthLevel level

boxesInRow :: (Coord -> Tile) -> Int -> [Coord]
boxesInRow maze r = foldAppendList (coordForTile maze Box r)

loadLevel :: Level -> State
loadLevel level = S initialDirection (nthInitialCoord level) (initialBoxPositions level) level Playing
    
initialCoord :: Coord
initialCoord = startCoord mazes
  where 
    startCoord :: [Maze] -> Coord
    startCoord (Maze ic m : ms) = ic


initialState :: State
initialState = loadLevel 0

winState :: Level -> [Coord] -> WinState
winState level cs
  | and $ mapList (isOnStorage (nthLevel level)) cs =
    if level == listLength mazes - 1
      then GameWon
      else LevelWon
  | otherwise = Playing

