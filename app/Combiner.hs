module Combiner where

import Graphics.Gloss

draw21times :: (Int -> Picture) -> Picture
draw21times something = Pictures $ go (-10)
  where
    go 11 = []
    go n = something n : go (n + 1)

combine :: [Picture] -> Picture
combine = Pictures



