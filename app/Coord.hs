module Coord where

import Graphics.Gloss
import Direction
import GraphicsUtil

data Coord = C Int Int
instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
  c1 /= c2 = not (c1 == c2)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translate (fromIntegral (1 * x) * s) (fromIntegral (1 * y) * s)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1)  y
adjacentCoord U (C x y) = C  x    (y+1)
adjacentCoord D (C x y) = C  x    (y-1)
adjacentCoord L (C x y) = C (x-1)  y
 
adjacentCoord2 :: Coord -> Direction -> Coord
adjacentCoord2 cin d = adjacentCoord d cin
