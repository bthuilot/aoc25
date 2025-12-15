
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
                     cx <- [xDist `div` (abs xDist)], cy <- [yDist `div` (abs yDist)] ]
parsePerimeter rest = rest

-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . maximum . (map calculateRectangeArea) . uniquePairs . parseRedTiles

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 = Ok . show . parsePerimeter . parseRedTiles
