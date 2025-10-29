{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

module Days.D00 (
  Days.D00.part1, Days.D00.part2
  ) where

import Data.Char
import AOC


-- Test Day

part1 :: String -> DayResult
part1 = Ok

part2 :: String -> DayResult
part2 = Ok . map toUpper
