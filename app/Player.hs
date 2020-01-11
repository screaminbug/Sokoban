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
  [translate 0 (0.3 * s) cranium
  ,bodyLine [(0,0),((0.3 * s), (0.05 * s))]
  ,bodyLine [(0,0),((0.3 * s),-(0.05 * s))]
  ,bodyLine [(0,(-0.2 * s)),(0,(0.1 * s))]
  ,bodyLine [(0,(-0.2 * s)),((0.1 * s),-(0.5 * s))]
  ,bodyLine [(0,(-0.2 * s)),(-(0.1 * s),-(0.5 * s))]]
  where cranium = Pictures [circle (0.18 * s), arcSolid (0.3 * s) 200 (0.18 * s)]
  
player L = scale (-1) 1 (player R)

player U = Pictures 
  [translate 0 (0.3 * s) cranium
  ,bodyLine [(0,0),((0.3 * s), (0.05 * s))]
  ,bodyLine [(0,0),(-(0.3 * s),(0.05 * s))]
  ,bodyLine [(0,(-0.2 * s)),(0,(0.1 * s))]
  ,bodyLine [(0,(-0.2 * s)),((0.1 * s),-(0.5 * s))]
  ,bodyLine [(0,(-0.2 * s)),(-(0.1 * s),-(0.5 * s))]]
  where cranium = circleSolid (0.18 * s)
  
player D = Pictures
  [translate 0 (0.3 * s) cranium
  ,bodyLine [(0,0),((0.3 * s), -(0.05 * s))]
  ,bodyLine [(0,0),(-(0.3 * s),-(0.05 * s))]
  ,bodyLine [(0,(-0.2 * s)),(0,(0.1 * s))]
  ,bodyLine [(0,(-0.2 * s)),((0.1 * s),-(0.5 * s))]
  ,bodyLine [(0,(-0.2 * s)),(-(0.1 * s),-(0.5 * s))]]
  where cranium = Pictures 
                  [circle (0.18 * s)
                  ,translate  (0.05 * s) (0.06 * s) (circleSolid (0.04 * s))
                  ,translate (-0.05 * s) (0.06 * s) (circleSolid (0.04 * s))]
                
playerInMaze :: Direction -> Coord -> Picture
playerInMaze direction coord = atCoord coord (player direction) 

movePlayer :: State -> IO State
movePlayer (S whichWay from boxes level isWon)
  | playerCanMoveIn (mazeWithBoxes' to) (mazeWithBoxes' boxTo) = movePlayerWithSound Walk $ S whichWay to moveBox level $ winState level moveBox
  | otherwise                                                  = movePlayerWithSound Bump $ S whichWay from boxes level isWon 
  where 
  to = adjacentCoord whichWay from
  boxTo = adjacentCoord whichWay to
  mazeWithBoxes' = mazeWithBoxes currentMaze boxes
  moveBox = mapList (moveFromTo to boxTo) boxes
  currentMaze = nthLevel level

movePlayerWithSound :: Sound -> State -> IO State
movePlayerWithSound Walk s@(S w f b l won) = do
  if won == LevelWon then playFromFile "./win.wav" else if 
  won == GameWon then playFromFile "./gamewon.wav" else playFromFile "./walk.wav"
  return s
movePlayerWithSound Bump s = do
  playFromFile "./bump.wav"
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

