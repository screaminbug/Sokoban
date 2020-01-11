module Tiles where

import Graphics.Gloss
import GraphicsUtil

data Tile = Wall | Ground | Storage | Box | Invalid deriving Eq

outline :: Picture
outline = color black $ thickLine 5 $ rectanglePath (s * 1) (s * 1)

wall :: Picture
wall = Pictures [color (greyN 0.5) (rectangleSolid (s * 1) (s * 1)), outline]

ground :: Picture
ground = Pictures[color yellow (rectangleSolid (s * 1) (s * 1)), outline]

storage :: Picture
storage = Pictures [ground, color white (circleSolid (s * 0.3))]

box :: Picture
box = Pictures [color red (rectangleSolid (s * 0.5) (s * 0.5))]

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Invalid = blank


