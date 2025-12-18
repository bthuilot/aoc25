
{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D09
Description :  Advent of Code 2025 day 9 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/9

-}

module Days.D09 (
  Days.D09.part1, Days.D09.part2
  ) where


import AOC ( DayResult(Ok) )


import Utils.Strings (wordsWhen)
import Utils.Lists (uniquePairs)
import GHC.Float (int2Double)
import Data.List (sort)

type Tile = (Int,Int)

parseRedTiles :: String -> [Tile]
parseRedTiles = map (parseLine . wordsWhen (==',')) . lines
  where
    parseLine [x,y] = (read x, read y)
    parseLine _ = error "invalid line"

calculateRectangeArea :: (Tile, Tile) -> Int
calculateRectangeArea ((x1,y1), (x2,y2)) = xDist * yDist
  where
    xDist = max x1 x2 - min x1 x2 + 1
    yDist = max y1 y2 - min y1 y2 + 1

parsePerimeter :: [Tile] -> [Tile]
parsePerimeter (tile@(x1,y1) : nextTile@(x2,y2) : next) = (tile : tilesBetween) ++ parsePerimeter (nextTile : next)
  where
    xDist = x2 - x1
    yDist = y2 - y1
    tilesBetween = [ (dx + cx * x1, dy + cy * y1) |
                     dx <- [0..(abs xDist)], dy <- [0..(abs yDist)],
                     cx <- [if xDist < 0 then (-1) else 1], cy <- [if yDist < 0 then (-1) else 1]]
--                        yDist `div` (abs yDist)] ]
parsePerimeter rest = rest

tileIsRedOrGreen :: [Tile] -> [Tile] -> Tile -> Bool
-- tileIsRedOrGreen _ [] _ = False
tileIsRedOrGreen p redTiles  tile@(px, py) = tile `elem` p
  || (odd . length . filter id $ zipWith intersects redTiles (tail $ cycle redTiles))
  where
    -- https://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
    intersects (x1,y1) (x2, y2) =
      (((y1 <= py) && (py < y2)) ||
      ((y2 <= py) && (py < y1)))
      &&
      ( (int2Double px) <
        (int2Double ((x2 - x1) * (py - y1))) /
        (int2Double (y2 - y1)) +
        (int2Double x1))
--      (y1 > py) /= (y2 > py) && px < (x2 - x1) * (py - y1) `div` (y2 - y1) + x1

calculateContainedArea :: [Tile] -> [Tile] -> (Tile, Tile) -> Int
calculateContainedArea p redTiles (c1@(x1, y1), c2@(x2, y2)) = if fullyContained then calculateRectangeArea (c1, c2) else 0
  where
    fullyContained = all (tileIsRedOrGreen p redTiles) (parsePerimeter [c1,c3,c2,c4])
                     -- all (tileIsRedOrGreen redTiles) [c1,c2,c3,c4]
                     -- && all (not . tileWithin c1 c2) redTiles
    (c3, c4) = ((x2,y1),(x1,y2))
    tileWithin (x1,y1) (x2, y2) (tx,ty) = tx < max x1 x2 && tx > min x1 x2 && ty < max y1 y2 && ty > min y1 y2


-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . maximum . (map calculateRectangeArea) . uniquePairs . parseRedTiles

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 i = Ok . show . take 15 . reverse . sort . (map (calculateContainedArea perimeter redTiles)) $ uniquePairs redTiles
  where
    perimeter = parsePerimeter redTiles
    redTiles = parseRedTiles i
