{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

module AOC
  (
    Day,
    DayResult(Ok, InvalidInput, NotImplemented),
    DaySolution,
    notImplemented,
    execDay,
  )
where

data DayResult = Ok String | InvalidInput | MissingInput | NotImplemented

-- | Show DayResult defines the 'show' command for 'DayResult'
instance Show DayResult where
  show (Ok str) = str
  show InvalidInput = "error - invalid input"
  show MissingInput = "error - unable to find input"
  show NotImplemented = "not implemented"

-- | DaySolution represents the solution function to
-- one part of the day's challenge.
type DaySolution = String -> DayResult

-- | notImplemented is a DaySolution that just returns NotImplemented
notImplemented :: DaySolution
notImplemented = const NotImplemented

-- | Day represents the solutions to a day's challenge.
-- A Day consists of two parts, each a Maybe DaySolution, which will compute
-- the solution to the day's part.
-- A Nothing part represents that the solution has not yet been implemented
type Day = [DaySolution]

-- | execDay executes both parts of the 'Day' with
-- the given input and prints results to STDOUT
execDay :: Day -> Maybe String -> IO ()
execDay _  Nothing = putStrLn "no input for day"
execDay day (Just input) = mapM_ (putStrLn . showSolution) $ zip [1..] filled
  where
    filled = day ++ replicate (2 - length day) notImplemented
    showSolution :: (Int, DaySolution) -> String
    showSolution (part, solution)  = "part " ++ show part ++ ": "  ++ show (solution input)
  
