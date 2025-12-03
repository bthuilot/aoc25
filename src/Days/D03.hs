{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D03
Description :  Advent of Code 2025 day 3 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/3

This day was easiest yet. It was just a matter of
figuring out that to find max joltage with X digits,
The first digit has to be selected from the from the first
length(digits) - (X - 1), since there should be at least X - 1 digits
left when you recur to select the next max digit.


i.e. if you are selecting the max joltage for 3 digits with
a list of digits 5 long, the first digit will be within the first 3,
since when you select the next digit there needs to be at least 2
digits.

-}

module Days.D03 (
  Days.D03.part1, Days.D03.part2
  ) where


import AOC ( DayResult(Ok) )
import Data.Char(digitToInt)


-- | 'BatterBank' represents 
type BatteryBank = [Int]

-- | 'parseInput' parses the input string
-- into a battery bank
parseInput :: String -> [BatteryBank]
parseInput = map (map digitToInt) . lines


-- | 'maxJoltage' finds the max joltage for a 'BatteryBank'
-- for a given length of the battery.
maxJoltage :: Int -> BatteryBank -> Int
maxJoltage 1 bank = foldl max 0 bank
maxJoltage len bank = (placeValueMultiplier * maxBattery) + recur
  where
    batteries = take (length bank - (len - 1)) bank -- possible batteries for current place value
    maxBattery = foldl max 0 batteries -- max battery for the current place value
    placeValueMultiplier = 10 ^ (len - 1) -- multipler for place value digit (i.e. 10s, 100s, 1000s, etc)
    recur = maxJoltage (len - 1) (drop 1 $ dropWhile (/= maxBattery) bank) -- recur on rest of digits


-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . sum . map (maxJoltage 2) . parseInput

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 = Ok . show . sum . map (maxJoltage 12) . parseInput
