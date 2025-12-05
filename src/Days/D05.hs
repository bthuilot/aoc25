{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D05
Description :  Advent of Code 2025 day 5 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/5

Another fairly easily day.
First part was just checking for
which ingredient IDs were apart of the given ranges
and counting how many.

Part two just involved first condensing
any ranges that may overlap, then counting
the how many elements are in each range.

-}

module Days.D05 (
  Days.D05.part1, Days.D05.part2
  ) where

import AOC (DayResult(Ok))

type Range = (Int, Int)

-- | 'inRange' will return true
-- if a given int is within the given
-- 'Range' and false otherwise
inRange :: Int -> Range -> Bool
inRange i (l, u) = i >= l && i <= u

-- | 'parseInput' will parse in the input string
-- into both a list of 'Range' and a list of
-- ingredient IDs
parseInput :: String -> ([Range], [Int])
parseInput i = (freshRanges, ids)
  where
    ids = map read $ drop 1 idsInput
    freshRanges = map (parseRange . break (== '-')) freshRangesInput
    parseRange (l,u) = (read l, read (drop 1 u))
    (freshRangesInput, idsInput) = break (== "") $ lines i

-- | 'isFresnIngredient' determines if a given ingrident
-- id is fresh based on a list of ranges
isFreshIngredient :: [Range] -> Int -> Bool
isFreshIngredient ranges i = any (inRange i) ranges

-- | 'condenseRanges' will take a list of ranges
-- and condense any overlapping ranges
condenseRanges :: [Range] -> [Range]
condenseRanges = foldl condense []
  where
    condense [] r = [r]
    condense (r@(l, u) : rs) (l', u')
      | max l l' <= min u u' = condense rs (min l l' , max u u')
      | otherwise = r : condense rs (l',u')

-- | 'itemsInRanges' counts how many ingredient
-- ids are within within a list of ranges.
-- NOTE: this assumes no two 'Range' overlap
itemsInRanges :: [Range] -> Int
itemsInRanges = foldl countRange 0
  where
    countRange acc (l,u) = acc + (u - l) + 1


-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 i = Ok $ show $ length $ filter (isFreshIngredient freshRanges) ids
  where
    (freshRanges, ids) = parseInput i


-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 i = Ok $ show $  itemsInRanges $ condenseRanges freshRanges
  where
    (freshRanges, _) = parseInput i
