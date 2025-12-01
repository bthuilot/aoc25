{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

module Days
  ( getDay
  , days
  , totalDays
  )
where

import AOC ( Day )
import qualified Days.D00 as D0
import qualified Days.D01 as D1


-- | getDay returns the day for the given int.
-- will panic if the date is negative or greater than
-- 'totalDays'
getDay :: Int -> Day
getDay 0 = [D0.part1, D0.part2]
getDay i
  | i > totalDays || i < 1 = error "invalid day given"
  | otherwise = days !! (i - 1)

-- | totalDays are the total amount of days
totalDays :: Int
totalDays = 12

-- | days is the list of all days for the advent of code challenge.
days :: [Day]
days =
  allDays ++ replicate (totalDays - length allDays) []
 where
   allDays = [
     [D1.part1, D1.part2]
     ]
     