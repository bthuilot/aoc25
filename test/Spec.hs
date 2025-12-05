{-
 Copyright (C) 2025 Bryce Thuilot

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the FSF, either version 3 of the License, or (at your option) any later version.
 See the LICENSE file in the root of this repository for full license text or
 visit: <https://www.gnu.org/licenses/gpl-3.0.html>.
-}

import AOC
import Data.List
import Data.Maybe
import Days
import System.Exit

main :: IO ()
main = do
  testResults <- mapM runTestCase testCases
  if any isFailed testResults then exitFailure else exitSuccess
  where
    isFailed (TestFail _) = True
    isFailed _ = False

testCases :: [TestCase]
testCases =
  [ TestCase
      { day = 0,
        part = 1,
        input = "hello, world!",
        expected = "hello, world!"
      },
    TestCase
      { day = 0,
        part = 2,
        input = "hello, world!",
        expected = "HELLO, WORLD!"
      },
    TestCase
      { day = 1,
        part = 1,
        input =
          "L68\n"
            ++ "L30\n"
            ++ "R48\n"
            ++ "L5\n"
            ++ "R60\n"
            ++ "L55\n"
            ++ "L1\n"
            ++ "L99\n"
            ++ "R14\n"
            ++ "L82\n",
        expected = "3"
      },
    TestCase
      { day = 1,
        part = 2,
        input =
          "L68\n"
            ++ "L30\n"
            ++ "R48\n"
            ++ "L5\n"
            ++ "R60\n"
            ++ "L55\n"
            ++ "L1\n"
            ++ "L99\n"
            ++ "R14\n"
            ++ "L82\n",
        expected = "6"
      },
    TestCase
      { day = 1,
        part = 2,
        input = "R1000\n",
        expected = "10"
      },
    TestCase
      { day = 2,
        part = 1,
        input =
          "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
            ++ "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
            ++ "824824821-824824827,2121212118-2121212124",
        expected = "1227775554"
      },
    TestCase
      { day = 2,
        part = 2,
        input =
          "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
            ++ "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
            ++ "824824821-824824827,2121212118-2121212124",
        expected = "4174379265"
      },
    TestCase
      { day = 3,
        part = 1,
        input =
          "987654321111111\n"
            ++ "811111111111119\n"
            ++ "234234234234278\n"
            ++ "81818191111211\n",
        expected = "357"
      },
    TestCase
      { day = 3,
        part = 2,
        input =
          "987654321111111\n"
            ++ "811111111111119\n"
            ++ "234234234234278\n"
            ++ "818181911112111\n",
        expected = "3121910778619"
      },
    TestCase
      { day = 4,
        part = 1,
        input =
          "..@@.@@@@.\n"
            ++ "@@@.@.@.@@\n"
            ++ "@@@@@.@.@@\n"
            ++ "@.@@@@..@.\n"
            ++ "@@.@@@@.@@\n"
            ++ ".@@@@@@@.@\n"
            ++ ".@.@.@.@@@\n"
            ++ "@.@@@.@@@@\n"
            ++ ".@@@@@@@@.\n"
            ++ "@.@.@@@.@.\n",
        expected = "13"
      },
    TestCase
      { day = 4,
        part = 2,
        input =
          "..@@.@@@@.\n"
            ++ "@@@.@.@.@@\n"
            ++ "@@@@@.@.@@\n"
            ++ "@.@@@@..@.\n"
            ++ "@@.@@@@.@@\n"
            ++ ".@@@@@@@.@\n"
            ++ ".@.@.@.@@@\n"
            ++ "@.@@@.@@@@\n"
            ++ ".@@@@@@@@.\n"
            ++ "@.@.@@@.@.\n",
        expected = "43"
      },
    TestCase
      { day = 5,
        part = 1,
        input =
          "3-5\n"
            ++ "10-14\n"
            ++ "16-20\n"
            ++ "12-18\n"
            ++ "\n"
            ++ "1\n"
            ++ "5\n"
            ++ "8\n"
            ++ "11\n"
            ++ "17\n"
            ++ "32",
        expected = "3"
      },
    TestCase
      { day = 5,
        part = 2,
        input =
          "3-5\n"
            ++ "10-14\n"
            ++ "16-20\n"
            ++ "12-18\n"
            ++ "\n"
            ++ "1\n"
            ++ "5\n"
            ++ "8\n"
            ++ "11\n"
            ++ "17\n"
            ++ "32",
        expected = "14"
      }
  ]

data TestCase = TestCase
  { day :: Int,
    part :: Int,
    input :: String,
    expected :: String
  }

data TestResult = TestPass | TestFail String | TestSkippedNotImplemeted | TestSkippedNoInput

printTestResult :: Int -> Int -> TestResult -> IO ()
printTestResult dayNum partNum tr = putStrLn output
  where
    output = "day " ++ show dayNum ++ " part " ++ show (partNum + 1) ++ ": " ++ showResult tr
    showResult TestPass = "pass"
    showResult TestSkippedNotImplemeted = "skipped - not implemeted"
    showResult TestSkippedNoInput = "skipped - no input"
    showResult (TestFail s) = "failed - " ++ s

runTestCase :: TestCase -> IO TestResult
runTestCase tc = do
  let testResult = evalResult (solution i)
  printTestResult dayNum partNum testResult
  return testResult
  where
    (i, e, dayNum, partNum) = (input tc, expected tc, day tc, part tc - 1)
    solution = fromMaybe notImplemented (getDay dayNum !? partNum)
    evalResult (Ok output) = if e == output then TestPass else TestFail (output ++ " did not match expected " ++ e)
    evalResult NotImplemented = TestSkippedNotImplemeted
    evalResult _ = TestFail "unable to run"
