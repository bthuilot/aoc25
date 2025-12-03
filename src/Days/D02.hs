{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D02
Description :  Advent of Code 2025 day 2 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/2

This problem was easier than day 1 imo.
I also misread the question and start to
work on part 2 so that also made it pretty easy.

My approach was to break the ID into groups
of a given size (1, 2, 3 etc.) and just check
if broken into those groups, all elements are equal.
I also new the max size a pattern could be was half
the length of the ID.

That made finding if an id is valid just:

for all numbers >= 1 and =< id length / 2
 - check if spliting ID characters into groupings
   of that size, are they all equal
 - in part 1, additionally check the length of groupings
   is equal to 2

Then I went through each number in the ranges and checked
for any invalid.

-}

module Days.D02 (
  Days.D02.part1, Days.D02.part2
  ) where


import AOC ( DayResult(Ok) )

import Utils.Strings (wordsWhen)
import Utils.Lists (groupsOf, allEqual)

-- | 'Range' represents an integer range.
-- The first integer is the lower bound and
-- the second is the upper bound
data Range = Range Int Int

-- | 'isInvalidID' will check if a product ID is invalid,
-- and return True. The first parameter can specify the max amount
-- of patterns to check for (or -1 to ignore). The second parameter
-- is the productID
isInvalidID :: Int -> Int -> Bool
isInvalidID maxPatternLen productID = any (repeatedPattern strID) [1..length strID `div` 2]
  where
    repeatedPattern str patternLen = let grouping = groupsOf patternLen str in
      allEqual grouping && (length grouping == maxPatternLen || maxPatternLen < 0)
    strID = show productID

-- | 'parseRanges' will parse a list of integer 'Range'
-- from the input according to the days challenge
parseRanges :: String -> [Range]
parseRanges = map parseRange . ranges 
  where
    parseRange r = case wordsWhen (=='-') r of
      l : u : _ -> Range (read l) (read u)
      _ -> error "invalid input"
    ranges = wordsWhen (==',')

-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . sumInvalidIDs .  parseRanges
  where
    findInvalidIDs (Range l u) = [productID | productID <- [l..u], isInvalidID 2 productID]
    sumInvalidIDs = sum . concatMap findInvalidIDs


-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 = Ok . show . sumInvalidIDs .  parseRanges
  where
    findInvalidIDs (Range l u) = [productID | productID <- [l..u], isInvalidID (-1) productID]
    sumInvalidIDs = sum . concatMap findInvalidIDs

