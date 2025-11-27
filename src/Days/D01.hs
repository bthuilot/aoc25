{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D01
Description :  Advent of Code 2025 day 1 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable


-}

module Days.D01 (
  Days.D01.part1, Days.D01.part2
  ) where

import Data.Char ( toUpper )
import AOC ( DayResult(Ok) )


-- Test Day

-- | 'part1' is the first part of the example solution
-- which just returns the input string
part1 :: String -> DayResult
part1 = Ok

-- | 'part2' is the second part of the example solution
-- which will convert all lower case letters in the input
-- string to uppercase.
part2 :: String -> DayResult
part2 = Ok . map toUpper
