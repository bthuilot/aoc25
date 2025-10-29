{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

module Main (main) where

import Control.Exception
import System.IO
import System.IO.Error
import System.Environment
import System.Exit
import Text.Read


import AOC
import Days

-- | usage prints the usage information to STDOUT
usage :: IO ()
usage = putStrLn "Usage: aoc25 [-h] [days ..]"

-- | pad will be a string to be 'amt' in length,
-- using 'chr'. If the string is longer than 'amt',
-- just the string is returned
pad :: Int -> Char -> String -> String
pad amt chr str = replicate (amt - length str) chr ++ str

showDayNum :: Int -> String
showDayNum = pad 2 '0' . show

-- | parseArgs will parse the CLI arguments for
-- the selected days to run. The function will
-- return a list of the selected days as a pairing
-- of the number selected and the 'Day' to run.
parseArgs :: [String] -> IO [(Int, Day)]
parseArgs ["-h"] = usage   >> exitSuccess
parseArgs []     = return $ zip [1..] days 
parseArgs selections   = do
  validDays <- validateDaySelections selections
  return $ zip validDays (map getDay validDays)

-- | validateDaySelections will validate all
-- input arguments are valid integers within the range of
-- available dates. If any one is not, the program is exited.
-- All converted integers are returned.
validateDaySelections :: [String] -> IO [Int]
validateDaySelections selections = mapM validateSelections parsed
  where
    parsed = zip selections (map readMaybe selections)
    validateSelections (selection, Nothing) = putStrLn ("invalid day number: " ++ selection) >> exitFailure 
    validateSelections (selection, Just d)
      | d > length days || d < 0 = putStrLn ("day number out of range: " ++ selection) >> exitFailure
      | otherwise = return d


putDayHeader :: Int -> IO ()
putDayHeader num = do
  let numStr = pad 2 '0' (show num)
  mapM_ putStrLn [ ""
    , "Day " ++ numStr
    ,  "======"]

readInputForDay :: Int -> IO String
readInputForDay i = do
  result <- try $ openFile ("./inputs/" ++ showDayNum i ++ ".txt") ReadMode
  either handleNotFound hGetContents result
  where
    handleNotFound e = if isDoesNotExistError e then return "" else ioError e

main :: IO ()
main = do
  selectedDays <- getArgs >>= parseArgs
  mapM_ (\(i,d) -> putDayHeader i >> readInputForDay i >>= execDay d) selectedDays
  
