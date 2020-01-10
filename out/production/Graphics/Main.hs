{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import State
import Tiles
import Coord
import Direction
import Activities
import Combiner
import Player
import Boxes
import Maze
import GraphicsUtil
import Audio
import Control.Monad (unless)



canvas :: Display
canvas = InWindow "Sokoban" (1080, 1080) (10, 10)

background :: Color
background = white

simSteps :: Int
simSteps = 15

exercise2 :: Activity State
exercise2 = Activity initialState handleEvent drawState

showWin :: WinState -> Picture
showWin isWon = case isWon of
  LevelWon -> text "Level complete!"
  GameWon  -> text "All done!"
  Playing  -> blank

staticPicture :: Picture -> IO()
staticPicture = display canvas background

gameActivity :: Activity state -> IO()
gameActivity = runActivity canvas background simSteps


main :: IO ()
main = do
  audioOk <- defaultAudio
  unless audioOk $ fail "failed to initialize the audio system"
--  gameActivity $ resettable $ withStartScreen exercise2
  gameActivity $ resettable exercise2
--main = staticPicture $ Pictures [translate 0 0 (drawTile Box), translate 100 0 (drawTile Storage), player D]
--main = staticPicture $ pictureOfMaze maze1
--main = staticPicture $ polygon $ offsetPoint (10, 50) (100, 200) 10
--main = staticPicture $ Pictures[polygon $ thickPath (100, 50) (10, 50) 20, color white (line [(10, 50), (100, 200)])]
--main = staticPicture $ color green (circleSolid 40)

updateWinState :: State -> State
updateWinState (S dir coord boxes level isWon) = case isWon of
  LevelWon -> loadLevel (level + 1)
  GameWon  -> S dir coord boxes level isWon

changeDirecton :: State -> Direction -> State
changeDirecton (S _ coord boxes level isWon) d = S d coord boxes level isWon

move :: Event -> State -> IO State
move (EventKey (SpecialKey KeyRight) Down _ _) s = movePlayer $ changeDirecton s R
move (EventKey (SpecialKey KeyUp)    Down _ _) s = movePlayer $ changeDirecton s U
move (EventKey (SpecialKey KeyLeft)  Down _ _) s = movePlayer $ changeDirecton s L
move (EventKey (SpecialKey KeyDown)  Down _ _) s = movePlayer $ changeDirecton s D
move _ s = return s

nextLevel :: Event -> State -> IO State
nextLevel (EventKey (SpecialKey KeySpace) Down _ _) s = return $ updateWinState s
nextLevel _                                         s = return s

handleEvent :: Event -> State -> IO State
handleEvent e s@(S _ _ _ _ isWon) = case isWon of
  Playing -> move e s
  _       -> nextLevel e s

drawState :: State -> IO Picture
drawState (S dir coord boxpos level isWon) =
  return $ Pictures
  [pictureOfMaze $ nthLevel level
  ,playerInMaze dir coord
  ,pictureOfBoxes boxpos
  ,showWin isWon]
