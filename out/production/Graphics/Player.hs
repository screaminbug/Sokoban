module Player (player, movePlayer, playerInMaze) where

import Graphics.Gloss

import Direction
import Coord
import Tiles
import Maze
import State
import ListUtil
import GraphicsUtil
import Combiner
import Audio

bodyLine :: Path -> Picture
bodyLine = thickLine 3

player :: Direction -> Picture
player R = Pictures 
  [translate 0 30 cranium
  ,bodyLine [(0,0),(30,5)]
  ,bodyLine [(0,0),(30,-5)]
  ,bodyLine [(0,-20),(0,10)]
  ,bodyLine [(0,-20),(10,-50)]
  ,bodyLine [(0,-20),(-10,-50)]]
  where cranium = Pictures [circle 18, arcSolid 30 200 18]
  
player L = scale (-1) 1 (player R)

player U = Pictures 
  [translate 0 30 cranium
  ,bodyLine [(0,0),(30,5)]
  ,bodyLine [(0,0),(-30,5)]
  ,bodyLine [(0,-20),(0,10)]
  ,bodyLine [(0,-20),(10,-50)]
  ,bodyLine [(0,-20),(-10,-50)]]
  where cranium = circleSolid 18
  
player D = Pictures
  [translate 0 30 cranium
  ,bodyLine [(0,0),(30,-5)]
  ,bodyLine [(0,0),(-30,-5)]
  ,bodyLine [(0,-20),(0,10)]
  ,bodyLine [(0,-20),(10,-50)]
  ,bodyLine [(0,-20),(-10,-50)]]
  where cranium = Pictures 
                  [circle 18
                  ,translate   5  6 (circleSolid 4)
                  ,translate (-5) 6 (circleSolid 4)]
                
playerInMaze :: Direction -> Coord -> Picture
playerInMaze direction coord = atCoord coord (player direction) 

movePlayer :: State -> IO State
movePlayer (S whichWay from boxes level isWon)
  | playerCanMoveIn (mazeWithBoxes' to) (mazeWithBoxes' boxTo) = movePlayerWithSound $ S whichWay to moveBox level $ winState level moveBox
  | otherwise                                                  = movePlayerWithSound $ S whichWay from boxes level isWon 
  where 
  to = adjacentCoord whichWay from
  boxTo = adjacentCoord whichWay to
  mazeWithBoxes' = mazeWithBoxes currentMaze boxes
  moveBox = mapList (moveFromTo to boxTo) boxes
  currentMaze = nthLevel level

movePlayerWithSound :: State -> IO State
movePlayerWithSound s = do
  playFromFile "./walk.wav"
  return s

moveFromTo :: Eq a => a -> a -> (a -> a)
moveFromTo from to toAdjust
  | from == toAdjust = to
  | otherwise        = toAdjust 


playerCanMoveIn :: Tile -> Tile -> Bool
playerCanMoveIn Ground  _   = True
playerCanMoveIn Storage _   = True
playerCanMoveIn Box Ground  = True
playerCanMoveIn Box Storage = True
playerCanMoveIn _ _         = False

