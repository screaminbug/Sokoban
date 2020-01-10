module Boxes (pictureOfBoxes) where

import Graphics.Gloss

import Coord
import Tiles
import Combiner
import Maze

someBoxCoords :: [Coord]
someBoxCoords = [C 2 2, C 3 3, C (-1) 0]

firstBox :: [Coord] -> Picture
firstBox [] = blank
firstBox (c:_) = atCoord c (drawTile Box)

pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes cs = combine (map (\c -> atCoord c (drawTile Box)) cs)


