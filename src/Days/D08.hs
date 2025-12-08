{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D08
Description :  Advent of Code 2025 day 8 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/8


-}

module Days.D08 (
  Days.D08.part1, Days.D08.part2
  ) where


import AOC ( DayResult(Ok, NotImplemented) )
import Utils.Strings (wordsWhen)


type JunctionBox = (Int, Int, Int)

parseJunctionBoxes :: String -> [JunctionBox]
parseJunctionBoxes = map parseBox . lines
  where
    parseBox b = tuplify $ map read $ wordsWhen (== ',') b
    tuplify [x, y, z] = (x,y,z)
    tuplify _ = error "invalid input"

-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . parseJunctionBoxes

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 _ = NotImplemented
