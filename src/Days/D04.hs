{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D04
Description :  Advent of Code 2025 day 4 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/4

This really wasn't hard at all.
Parse the input into a matrix, then
for each index, check if more than 3
of the surrounding 8 are paper rolls
and count how many you find.

For part 2, set each spot you find to
empty and run again. Keep track of how
many spots you find each time until
you find no spots.

-}

module Days.D04 (
  Days.D04.part1, Days.D04.part2
  ) where

import Data.Array ( Array, array, indices, (!), (//), bounds )

import AOC (DayResult(Ok, NotImplemented))

-- | 'Landamark' represents either an empty floor
-- spot, or one with a roll of paper
data Landmark = Empty | Paper
  deriving (Show, Eq)

-- | 'PrintingDeptDiagram' represents the floor diagram
-- of the printing department. Each index has a 'Landmark'
-- indicating what is at that spot.
type PrintingDeptDiagram = Array (Int, Int) Landmark

-- | 'parseLandmark' parses a character in the input string
-- into a 'Landmark'
parseLandmark :: Char -> Landmark
parseLandmark '.' = Empty
parseLandmark '@' = Paper
parseLandmark _ = error "invalid char"

-- | 'parseDiagram' will parse an input string into a diagram
parseDiagram :: String -> PrintingDeptDiagram
parseDiagram i = array ((0,0), (nRows - 1, nCols - 1)) (parseRows 0 l)
  where
    l = lines i
    (nRows, nCols) = (length l, length (head l))
    -- | parseRows will foldl but additionally to the accumlator
    -- pass in the row index
    parseRows _ [] = []
    parseRows rI (r : rs) = parseCol (rI, 0) r ++ parseRows (rI + 1) rs
    -- | parseCol will foldl but additionally to the accumulator
    -- pass a tuple of (row index, col index)
    parseCol _ [] = []
    parseCol (rI, cI) (c : cs) = ((rI, cI), parseLandmark c) : parseCol (rI, cI + 1) cs

-- | 'canForkliftAccess' will deteremine if a fork lift can access
-- a floor tile of the diagram (assuming the floor spot is a roll of paper)
canForkliftAccess :: PrintingDeptDiagram -> (Int,Int) -> Bool
canForkliftAccess diagram (row,col) = length (filter id surroundingsPaper) < 4
  where
    (_, (maxR, maxC)) = bounds diagram
    surroundingsPaper = [ne, n, nw, e, w, se, s, sw]
    ne = row > 0 && col > 0 && (diagram ! (row - 1,col -1)) == Paper
    n = row > 0 && (diagram ! (row - 1,col)) == Paper
    nw = row > 0 && col < maxC && (diagram ! (row - 1,col + 1)) == Paper
    e = col > 0 && (diagram ! (row,col -1)) == Paper
    w = col < maxC && (diagram ! (row,col +1)) == Paper
    s = row < maxR && (diagram ! (row + 1, col)) == Paper
    se = row < maxR && col > 0 && (diagram ! (row + 1,col -1)) == Paper
    sw = row < maxR && col < maxC && (diagram ! (row + 1,col +1)) == Paper
    

-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 i = Ok . show . length . accessablePapers $ indices diagram
  where
    diagram = parseDiagram i
    accessablePapers = filter (\idx -> (diagram ! idx) == Paper && canForkliftAccess diagram idx)


-- | 'clearDeptFloor' will contiually find all accessible paper rolls
-- and remove them until no more can be removed. It will record how many
-- rolls it clears each iteration and return the sum.
clearDeptFloor :: PrintingDeptDiagram -> Int
clearDeptFloor d = if amountRemoved == 0 then 0 else amountRemoved + recur
  where
    accessablePapers = filter (\idx -> (d ! idx) == Paper && canForkliftAccess d idx) (indices d)
    amountRemoved = length accessablePapers
    recur = clearDeptFloor (removePapers d accessablePapers)

-- | 'removePapers' removes the given rolls of paper (by their coordinates) from
-- diagram.
removePapers :: PrintingDeptDiagram -> [(Int, Int)] -> PrintingDeptDiagram
removePapers d idxs = d // map (\idx -> (idx, Empty)) idxs


-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 i = Ok . show $ clearDeptFloor diagram
  where
    diagram = parseDiagram i

