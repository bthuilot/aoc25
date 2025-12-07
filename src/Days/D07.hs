{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

{- |
Module      :  D07
Description :  Advent of Code 2025 day 7 implementation
Copyright   :  (c) Bryce Thuilot <bryce@thuilot.io>
License     :  GPL-3.0-or-later

Maintainer  :  bryce@thuilot.io
Stability   :  frozen
Portability :  portable

https://adventofcode.com/2025/day/7

Tricky one. Took me a while to even
figure out how part 1 was calculated.

I mainly took the approach of iterating
through each row with the list of beams
and "spliting" for each beam that hit a '^'
within the row.

Part 1 then just involved counting the amount of
splits each row and summing them.

Part 2 required me to record an "intensity"
for each beam. When two beams share a position,
I "combine" the beams into one in the list and
increase the intensity by 1. Once the manifold
was iterated through, sum the intensity of each
beam to find the amount of timelines.


-}

module Days.D07 (
  Days.D07.part1, Days.D07.part2
  ) where


import AOC ( DayResult(Ok) )

import Data.Array ( Array, array, (!), bounds )

import Data.List (elemIndex, nub)

-- | 'Position' represents what is at a 
-- coordinate in the tachyon manifold
data Position = Empty | Splitter
  deriving (Show, Eq)

parsePosition :: Char -> Position
parsePosition '.' = Empty
parsePosition 'S' = Empty
parsePosition '^' = Splitter
parsePosition _ = error "invalid input"

-- | Coordinate represents the coordinate of
-- a 'Position' in the 'TachyonManifold'
type Coordinate = (Int, Int)

-- | 'TachyonManifold' represents the diagram of
-- the tachyon manifold. each array index is the cooridnates
-- of a 'Position', which represents what is located at that coordinate
type TachyonManifold = Array Coordinate Position

-- | 'Beam' represents a beam traveling through
-- the 'TachyonManifold'. It has a horiztonal position (x coordinate)
type Beam = Int

-- | 'BeamWithItensity' represents a beam traveling through
-- the 'TachyonManifold'. It is the same as 'Beam' but additionally 
-- has an intensity value representing overlapping beams
type BeamWithIntensity = (Int,Int)

-- | 'parseManifold' will parse an input string into 
-- a tuple of 'TachyonManifold' and the starting
-- beams
parseManifold :: String -> (TachyonManifold, Int)
parseManifold i = (manifold, starting)
  where
    l = lines i
    (nRows, nCols) = (length l, length (head l))
    manifold = array ((0,0), (nRows - 1, nCols - 1)) (parseRows 0 l)
    starting = case elemIndex 'S' (head l) of
      Just x -> x
      Nothing -> error "invalid input"
    -- | parseRows will foldl but additionally to the accumlator
    -- pass in the row index
    parseRows _ [] = []
    parseRows rI (r : rs) = parseCol (rI, 0) r ++ parseRows (rI + 1) rs
    -- | parseCol will foldl but additionally to the accumulator
    -- pass a tuple of (row index, col index)
    parseCol _ [] = []
    parseCol (rI, cI) (c : cs) = ((rI, cI), parsePosition c) : parseCol (rI, cI + 1) cs


-- | 'countSplits' counts the amount of splits that happen
-- when running beams throught the 'TachyonManifold'.
-- the first argument is the row index and the second is a list
-- of 'Beam' to run through the manifold.
countSplits :: TachyonManifold -> Int -> [Beam] -> Int
countSplits manifold row beams
  | row > maxR = 0
  | otherwise = count + countSplits manifold (row + 1) (nub nextBeams)
  where
    (_, (maxR, maxC)) = bounds manifold
    splitBeam b = [ s | s <- [b + 1, b-1], s >= 0 && s <= maxC]
    (count, nextBeams) = foldl
    -- this is a little bit of a mess but the TLDR is:
    -- it recieves an accumaltor of the count of splits
    -- this round 
      (\(splits, beamAcc) b -> case  manifold ! (row, b) of
          Splitter -> (splits + 1, beamAcc ++ splitBeam b)
          _ -> (splits, beamAcc ++ [b]))
      (0, []) beams

-- | 'condenseBeamsIntensity' will condense a list of 'BeamWithIntensity'
-- by summing the intensities of beams with the same position.
condenseBeamsIntensity :: [BeamWithIntensity] -> [BeamWithIntensity]
condenseBeamsIntensity [] = []
condenseBeamsIntensity (beam : beams) = condense beam (condenseBeamsIntensity beams)
  where
    condense b [] = [b]
    condense b@(pos, i) (b'@(pos', i') : bs')
      | pos == pos' = condense (pos, i + i') bs'
      | otherwise = b' : condense b bs'
    

-- | 'countTimelines' will return the amount of timelines that are possible
-- when running a set of beams through the manifold. It has the same arguments
-- as 'countSplits' but takes a 'BeamWithIntensity' instead of 'Beam'
countTimelines :: TachyonManifold -> Int -> [BeamWithIntensity] -> Int
countTimelines  manifold row beams
  | row > maxR = foldl (\acc (_, i) -> i + acc) 0 $ beams
  | otherwise = countTimelines manifold (row + 1) (condenseBeamsIntensity nextBeams)
  where
    (_, (maxR, maxC)) = bounds manifold
    splitBeam (pos,i) = [ (s,i) | s <- [pos + 1, pos-1], s >= 0 && s <= maxC]
    nextBeams = foldl
      (\beamAcc b@(pos,_) -> case manifold ! (row, pos) of
          Splitter -> beamAcc ++ splitBeam b
          _ -> b : beamAcc
      )
      [] beams


-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 i = Ok . show $ countSplits manifold 0 [startingBeamPosition]
  where
    (manifold, startingBeamPosition) = parseManifold i

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 input = Ok . show $ countTimelines manifold 0 [(startingBeamPosition, 1)]
  where
    (manifold, startingBeamPosition) = parseManifold input
