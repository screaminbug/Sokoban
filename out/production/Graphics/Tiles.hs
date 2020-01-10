module Tiles where

import Graphics.Gloss
import GraphicsUtil

data Tile = Wall | Ground | Storage | Box | Invalid deriving Eq

outline :: Picture
outline = color black $ thickLine 5 $ rectanglePath 100 100

wall :: Picture
wall = Pictures [color (greyN 0.5) (rectangleSolid 100 100), outline]

ground :: Picture
ground = Pictures[color yellow (rectangleSolid 100 100), outline]

storage :: Picture
storage = Pictures [ground, color white (circleSolid 30)]

box :: Picture
box = Pictures [color red (rectangleSolid 50 50)]

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Invalid = blank


