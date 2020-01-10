module Audio where

import Sound.ProteaAudio

defaultAudio = initAudio 5 44100 256

playFromFile :: String -> IO()
playFromFile f = do
  sample <- sampleFromFile f 1.0 
  soundPlay sample 1 1 0 1
