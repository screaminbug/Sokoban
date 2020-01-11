module GraphicsUtil where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import ListUtil

s :: Float
s = 50

addV :: Vector -> Vector -> Vector
addV (a,b) (c,d) = (a + c, b + d)

thickPath :: Point -> Point -> Float -> Path
thickPath p1@(x1, y1) p2@(x2, y2) thickness =
  [calc p1 dist, calc p1 (-dist), calc p2 (-dist), calc p2 dist ]
  where
    dist = thickness / 2
    n = normalizeV (y1 - y2, x2 - x1)
    calc v d = v `addV` mulSV d n

thickLine :: Float -> Path -> Picture
thickLine thickness path = Pictures $ map pathSegToPoly $ chunk 2 path
  where
    pathSegToPoly p = Polygon $ process (head p) (last p)
    process :: Point -> Point -> [Point]
    process a b = thickPath a b thickness
    offs = thickness / 2
