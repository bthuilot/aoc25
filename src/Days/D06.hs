{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D06
Description :  Advent of Code 2025 day 6 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/6

Todays problem required a bit of clever parsing.

The first part was fairly easy, break the input into
lines and then split into words. A word at position `N`
will be in the same operation as every other word in
position `N` for the other lines. Transpositing the matrix
of words will associate the operations together with the
operation ('+' or '*') being the last word.

The second part was a bit trickier. This time,
I split the last line of operators first. I then
would parse each operator one at a time and measure
the distance from the current operator to the next.
That distance told me how many characters to take
from each other line for the same operator. I then
would transpose the substrings taken from each line
in order to to get the numbers parsed veritically.


-}

module Days.D06 (
  Days.D06.part1, Days.D06.part2
  ) where

import AOC (DayResult(Ok))

import Data.List (transpose)

import Utils.Strings (isAllSpaces)

-- | 'Operation' represents an operation to perform
data Operation = Add [Int] | Mul [Int]
  deriving (Show, Eq)

-- | 'execOperation' will execute the 'Operation'
-- and return the result
execOperation :: Operation -> Int
execOperation (Add operands) = sum operands
execOperation (Mul operands) = product operands

-- | 'parseOperationsHorizontally' will parse the operations
-- from the input by reading each number horizontally
-- i.e. 123 + 4 in 
-- 123
-- ---
--   4
--   +
parseOperationsHorizontally :: String -> [Operation]
parseOperationsHorizontally input = map (\x -> parse (last x)
                                   (map read (init x))) operations
  where
    operations = transpose $ map words $ lines input
    parse "+" operands = Add operands
    parse "*" operands = Mul operands
    parse _ _ = error "invalid operand"

-- | 'parseOperationsVertically' will parse the operations
-- from the input by read each number vertically
-- e.g. 1 + 2 + 34 in 
-- 1|2|3|
--  | |4|
--      +
parseOperationsVertically :: String -> [Operation]
parseOperationsVertically i = parseVerticalOperations operators nums
  where
     ls = lines i
     (operators, nums) = (last ls, init ls)

-- | 'parseOperations' will parse the vertical operations
-- from the operator line of input (given as the first input), and the
-- lines of number.
-- e.g.
-- input: "+  + *" ["12 5 6", " 4 6 4"]
-- output: [(Add [1, 24]), (Add [5,6]) (Mul [4,6])]
parseVerticalOperations :: String -> [String] -> [Operation]
parseVerticalOperations "" _ = []
parseVerticalOperations ops@(op : rs) nums = operation : recur
  where
    -- length till the next operation from current
    nextOpLength = length (op : takeWhile (\o -> o /= '*' && o /= '+') rs)
    -- operands for the current operation
    operands = filter (not . isAllSpaces) $ transpose $ map (take nextOpLength) nums
    recur = parseVerticalOperations (drop nextOpLength ops) (map (drop nextOpLength) nums)
    operation = parseOp op (map read operands)
    parseOp '*' ns = Mul ns
    parseOp '+' ns = Add ns
    parseOp _ _ = error "invalid operation"
    
-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . sum . map execOperation . parseOperationsHorizontally

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 = Ok . show . sum . map execOperation . parseOperationsVertically
