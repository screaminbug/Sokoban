module Activities where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import Coord
import Combiner
import Direction
import Screens

data SSState world = StartScreen | WinScreen (IO world) | Running world
data Activity world = Activity {state  :: world
                               ,handle :: Event -> world -> IO world
                               ,draw   :: world -> IO Picture}
  


resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (EventKey (SpecialKey KeyEsc) Down _ _) _ = pure state0
        handle' e s                                       = handle e s
    
--withStartScreen:: Activity s -> Activity (SSState s)
--withStartScreen (Activity state0 handle draw) 
--  = Activity state0' handle' draw'
--  where
--    state0' = StartScreen
--
--    handle' (EventKey (SpecialKey KeySpace) Down _ _) StartScreen = pure $ Running state0
--    handle' _              StartScreen          = StartScreen
--    handle' e              (Running s)          = Running (handle e s)
--
--    draw'                  StartScreen          = startScreen
--    draw'                  (Running s)          = draw s

    
runActivity :: Display -> Color -> Int -> Activity s -> IO ()
runActivity disp background steps (Activity state0 handle draw)
  = playIO disp background steps state0 draw handle $ \t w -> return w

