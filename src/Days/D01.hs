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

https://adventofcode.com/2025/day/1

Day 1 was definitetly more challenging than past years.
Part 1 was easy enough but I ended up making a bunch of
little mistakes while solving part 2 that defininetly slowed me down.

The final solution ended up being pretty simple:

For part 1, check if the amount to rotate is equal to
the distance from the dial to zero in the same direction.

For part 2, check if the amount to rotate is equal or greater
to the distance from the dail to zero, and additionally if its greater,
check how many times the overflow can evenly divide into the length
of the dial (i.e. 100).

-}

module Days.D01 (
  Days.D01.part1, Days.D01.part2
  ) where


import AOC ( DayResult(Ok) )


-- | 'DialTurn' represents an instruction
-- to turn the dial a given amount either
-- right of left
data DialTurn = L Integer | R Integer
  deriving (Eq, Show)


-- | 'dialStartingPoint' is the starting point of the dial
dialStartingPoint :: Integer
dialStartingPoint = 50

-- | 'dialLength' is the length of the dial
-- [0, 99] = 100
dialLength :: Integer
dialLength = 100

-- | getTurnAmount returns the amount the 'DialTurn'
-- will turn the dial
getTurnAmount :: DialTurn -> Integer
getTurnAmount (L amt) = amt
getTurnAmount (R amt) = amt

-- | parseInstruction parses a 'DialTurn'
-- instruction from a line of the input string
parseInstruction :: String -> DialTurn
parseInstruction ('L' : amt) = L $ read amt
parseInstruction ('R' : amt) = R $ read amt
parseInstruction _ = error "invalid input"

-- | parseInstructions parses 'DialTurn' instructions
-- from the input
parseInstructions :: String -> [DialTurn]
parseInstructions = map parseInstruction . lines

-- | 'turnDial' will turn the dial a given amount
-- (wrapping around if the dial passes zero) and return
-- the new dial
turnDial :: DialTurn -> Integer -> Integer
turnDial turn dial = rotate turn `mod` dialLength
  where
    rotate (L amt) = dial - amt
    rotate (R amt) = dial + amt

-- | 'PasswordMethod' represents a function that calculates
-- the additional password count method for a given dial turn.
-- it is given the 'DialTurn', the current dial and the result
-- of turning the dial (i.e. the next dial
type PasswordMethod = DialTurn -> Integer -> Integer -> Integer

-- | 'exactPasswordMethod' is the password method for counting
-- only when the dail reaches zero exactly as a result of the
-- a turn
exactPasswordMethod :: PasswordMethod
exactPasswordMethod _ _ 0 = 1
exactPasswordMethod _ _ _ = 0

-- | 'x434C49434B' is the password method "0x434C49434B"
-- described in the prompt. it will determine how many times
-- zero is passed when turning the dial.
x434C49434B :: PasswordMethod
x434C49434B turn dial _ =
  -- check to see if we make the dist from
  -- current dial to zero
  (if amt >= dist && dist /= 0 then 1 else 0)
  +
  -- additionally see how many more times it
  -- passes zero after that
  (if overflow > 0 then overflow `div` dialLength else 0)
    where
      amt = getTurnAmount turn -- amt to turn
      dist = distToZero turn dial -- distance from current dial to zero
      overflow = amt - dist -- additional distance after first zero pass
      distToZero (L _) d = d 
      distToZero (R _) d = (dialLength - d) `mod` 100
  


-- | 'crackSafe' will crack the password to a safe using a given
-- 'PasswordMethod' and list of 'DialTurns'
crackSafe :: PasswordMethod -> [DialTurn] -> Integer
crackSafe pwdMethod = snd . foldl countPassword (dialStartingPoint, 0)
  where
    countPassword (dial, password) turn =
      let nextDial = turnDial turn dial in
        (nextDial, password + pwdMethod turn dial nextDial)
    

-- | 'part1' is the solution to the first part of
-- the days challenge.
part1 :: String -> DayResult
part1 = Ok . show . crackSafe exactPasswordMethod . parseInstructions

-- | 'part2' is the solution to the second part of
-- the days challenge.
part2 :: String -> DayResult
part2 = Ok . show . crackSafe x434C49434B . parseInstructions
